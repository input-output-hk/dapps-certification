{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE LambdaCase           #-}

module Plutus.Certification.PdfInvoice
( generateInvoiceFile
, generateUnprocessedInvoices
, HasInvoiceTools
, HasInvoiceOutputFolder(..)
, HasWeasyPrintPath(..)
, syncAllInvoices
, PdfInvoiceSelector(..)
, renderPdfInvoiceSelector
) where

import Prelude as P

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask)
import Plutus.Certification.Internal
import Control.Monad.RWS (MonadReader (ask))
import System.Process (readProcessWithExitCode)
import GHC.IO.Exception (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import Plutus.Certification.Htmx (renderInvoice)
import System.FilePath
import System.Directory
import Observe.Event
import Observe.Event.Render.JSON


import qualified IOHK.Certification.Persistence as DB
import qualified Data.Text.IO as TIO
import Data.List (isSuffixOf)
import Data.HashSet (fromList,member)
import Data.Aeson (ToJSON(toJSON))
import Data.Aeson.QQ

-- synchronize unprocessed invoices - based on active subscriptions

class HasInvoiceOutputFolder env where
  getDestinationFolder :: env -> DestinationFolder
class HasWeasyPrintPath env where
  getWeasyPrintPath :: env -> FilePath

class (HasInvoiceOutputFolder env, HasWeasyPrintPath env) => HasInvoiceTools env where

syncAllInvoices :: (MonadIO m
                   , MonadMask m
                   , MonadReader env m
                   , HasInvoiceTools env
                   , HasDb env
                   )
                => EventBackend m r PdfInvoiceSelector
                -> m ()
syncAllInvoices eb = withEvent eb SyncAllInvoices \ev -> do
  dest <- getDestinationFolder <$> ask
  --_ <- getWeasyPrintPath <$> ask
  invoices <- withDb DB.getAllInvoiceIds

  -- read all files in destination folder
  liftIO $ createDirectoryIfMissing True dest
  files <- liftIO $ listDirectory dest
  -- filter out all non pdf files and remove extension
  let pdfFiles = map dropExtension $ filter (".pdf" `isSuffixOf`) files
  -- convert to Int64
  let foundIds = map read pdfFiles

  -- filter out all invoices that are already in the destination folder
  let fileSet = fromList foundIds
  let unprintedInvoices = filter (not . (`member` fileSet) . DB.fromId) invoices

  addField ev $ SyncAllInvoicesFieldFoundFiles $ length foundIds
  addField ev $ SyncAllInvoicesFieldUnprintedInvoices $ length unprintedInvoices

  -- generate all unprinted invoices
  mapM_ (generateInvoiceFile eb) unprintedInvoices

-- >>> filter (".pdf" `isSuffixOf`) <$> listDirectory "invoices"
-- ["1.pdf"]

-- | Generate all unprocessed invoices
generateUnprocessedInvoices :: ( MonadIO m
                               , MonadMask m
                               , MonadReader env m
                               , HasDb env
                               , HasInvoiceTools env
                               )
                            => EventBackend m r PdfInvoiceSelector
                            -> m ()
generateUnprocessedInvoices eb = withDb DB.getUnprintedInvoices
  >>= mapM_ (generateInvoiceFile eb)

type DestinationFolder = FilePath

data InvoiceGenerationResp
  = InvoiceNotFound
  | PdfGenerationFailed (ExitCode,String,String)
  | InvoiceGenerated FilePath


-- | Generate one invoice to pdf
generateInvoiceFile :: ( MonadIO m
                       , MonadMask m
                       , MonadReader env m
                       , HasDb env
                       , HasInvoiceTools env
                       )
                    => EventBackend m r PdfInvoiceSelector
                    -> DB.InvoiceId
                    -> m InvoiceGenerationResp
generateInvoiceFile eb invId' = withEvent eb GenerateInvoiceFile  \ev -> do
  withSystemTempDirectory "invoice-pdf-generation" \dir -> do
    addField ev $ GenerateInvoiceFileFieldInvoiceId invId'
    outputFolder <- getDestinationFolder <$> ask
    liftIO $ createDirectoryIfMissing True outputFolder
    -- 1. generate invice.html
    invM  <- withDb ( DB.getInvoice invId' )
    ret <- case invM of
      Nothing -> pure InvoiceNotFound
      Just inv -> do
        let html = renderInvoice inv
        -- write html to file
        let srcFile = dir </> "invoice.html"
        let destFile = outputFolder </> show (DB.fromId $ inv.invDtoParent.invId) <> ".pdf"
        liftIO $ TIO.writeFile srcFile html

        -- 3. convert to pdf
        weasyPrintPath <- getWeasyPrintPath <$> ask
        (exitCode',stdOut,stdError) <- liftIO $ readProcessWithExitCode
          weasyPrintPath [srcFile, destFile] ""
        case exitCode' of
          ExitFailure _ -> pure $ PdfGenerationFailed (exitCode',stdOut,stdError)
          ExitSuccess -> do
            -- 3. increase printed counter
            _ <- withDb $ DB.increaseInvoicePrintCounter invId'
            pure $ InvoiceGenerated destFile
    addField ev $ GenerateInvoiceFileFieldResp ret
    pure ret
--------------------------------------------------------------------------------
-- | INSTRUMENTATION

data PdfInvoiceSelector f where
  SyncAllInvoices :: PdfInvoiceSelector SyncAllInvoicesField
  GenerateInvoiceFile ::PdfInvoiceSelector GenerateInvoiceFileField

data SyncAllInvoicesField
  = SyncAllInvoicesFieldFoundFiles Int
  | SyncAllInvoicesFieldUnprintedInvoices Int

data GenerateInvoiceFileField
  = GenerateInvoiceFileFieldResp InvoiceGenerationResp
  | GenerateInvoiceFileFieldInvoiceId DB.InvoiceId


renderPdfInvoiceSelector :: RenderSelectorJSON PdfInvoiceSelector
renderPdfInvoiceSelector SyncAllInvoices = ("sync-all-invoices", \case
  SyncAllInvoicesFieldFoundFiles count -> ("found-files", toJSON count)
  SyncAllInvoicesFieldUnprintedInvoices count -> ("unprinted-invoices", toJSON count)
  )
renderPdfInvoiceSelector GenerateInvoiceFile = ("generate-invoice-file", \case
  GenerateInvoiceFileFieldInvoiceId invId -> ("invoice-id", toJSON invId)
  GenerateInvoiceFileFieldResp resp -> ("response", case resp of
    InvoiceNotFound -> toJSON ("invoice-not-found" :: String)
    PdfGenerationFailed (exitCode',stdOut,stdError) ->
      let exitCodeValue = case exitCode' of
            ExitFailure code -> toJSON code
            ExitSuccess -> toJSON (0 :: Int)
      in [aesonQQ| {
        "exit-code": #{exitCodeValue} 
        , "stdout": #{stdOut}
        , "stderr": #{stdError}
      }|]
    InvoiceGenerated destFile -> toJSON destFile
    )
  )
