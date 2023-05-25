import { useEffect, useState, useCallback } from "react";
import { Address } from "@emurgo/cardano-serialization-lib-browser";
import { useAppDispatch } from "store/store";
import { getProfileDetails, logout, setNetwork } from "store/slices/auth.slice";

import Modal from "components/Modal/Modal";
import Button from "components/Button/Button";
import Loader from "components/Loader/Loader";

import './ConnectWallet.scss';
import { fetchData } from "api/api";
import { AxiosResponse } from "axios";

const wallets: Array<string> = ['lace', 'nami', 'yoroi']

declare global {
    interface Window {
        cardano:any;
    }
}
let CardanoNS = window.cardano;

const ConnectWallet = () => {
    const dispatch = useAppDispatch();
    const [wallet, setWallet] = useState(null)
    const [walletName, setWalletName] = useState("")
    const [address, setAddress] = useState("")
    const [isOpen, setIsOpen] = useState(false)
    const [errorToast, setErrorToast] = useState<{display: boolean; statusText?: string; message?: string; showRetry?: boolean}>({display: false});
    const [walletLoading, setWalletLoading] = useState(false)

    const openConnectWalletModal = useCallback(() => {
        setErrorToast({display: false})
        setWallet(null)
        setIsOpen(true)
    },[])

    const onCloseModal = useCallback(() => setIsOpen(false),[]) 

    const loadWallet = async (walletName: string) => {
        try {
            setWalletLoading(true)
            const enabledWallet = await CardanoNS[walletName].enable();
            enabledWallet.getNetworkId().then(async (data: number) => {
                dispatch(setNetwork(data))
                setWallet(enabledWallet)
                setWalletName(walletName)
                if (enabledWallet) {
                    const response = await enabledWallet.getChangeAddress()
                    const walletAddr = Address.from_bytes(Buffer.from(response, "hex")).to_bech32()
                    initiatePrivateWalletSignature(enabledWallet, walletAddr, response)
                }
            })
        } catch (e) { handleError(e); }
    }

    const handleError = (error: any) => {
        if (error.info) {
            setErrorToast({
                display: true, 
                message: error.info, 
                showRetry: error.code === 3})
        } else if (error.response) {
            setErrorToast({
                display: true, 
                statusText: error.response.statusText, 
                message: error.response.data || undefined,
                showRetry: error.status === 403
            })
        } else {
          setErrorToast({display: true})
        }
    }

    const catchError = (err: any) => {
        handleError(err)
        setWalletLoading(false)
        dispatch(logout())
    }

    const initiatePrivateWalletSignature = async (currentWallet: any, walletAddr_bech32: any, walletAddr: string) => {
        const timestamp = (await fetchData.get<any,AxiosResponse<number>,any>('/server-timestamp')).data;
        const msgToBeSigned = `Sign this message if you are the owner of the ${walletAddr_bech32} address. \n Timestamp: <<${timestamp}>> \n Expiry: 60 seconds`;
        try {
            const {key, signature} = await currentWallet.signData(walletAddr, Buffer.from(msgToBeSigned, 'utf8').toString('hex'))
            if (key && signature) {
                const token = (await fetchData.post<any,AxiosResponse<string>,any>('/login', {
                    address: walletAddr_bech32,
                    key: key,
                    signature: signature
                })).data;
                localStorage.setItem('authToken', token)
                setAddress(walletAddr_bech32)
            } else {
                catchError({ message: "Could not obtain the proper key and signature for the wallet. Please try connecting again." })
            }
        } catch (err) {
            catchError(err)
        }
    }

    useEffect(() => {
        if (address) {
            (async () => {
                try {
                    await dispatch(getProfileDetails({"address": address, "wallet": wallet, "walletName": walletName})).catch(handleError)
                    setWalletLoading(false)
                } catch(error) {
                    setWalletLoading(false)
                    handleError(error)
                    // dispatch(clearCache()) 
                }
            })()
        }
    }, [dispatch, address, wallet, walletName])

    return (
        <>
            <Button
                type="button"
                displayStyle="gradient"
                buttonLabel={"Connect Wallet"}
                onClick={openConnectWalletModal}
            />
            <Modal open={isOpen} title="Connect a wallet" onCloseModal={onCloseModal}>
                <div id="walletsContainer">
                    {wallet ? 
                        null : !CardanoNS ? (<span>No wallet extensions installed yet!</span>) :
                        wallets.map((wallet: string, index: number) => {
                            if (CardanoNS && CardanoNS[wallet]) {
                                return (
                                    <div className="card" key={index} onClick={(_) => loadWallet(wallet)}>
                                        <img src={CardanoNS[wallet].icon} alt={CardanoNS[wallet].name} />
                                        <span>{CardanoNS[wallet].name}</span>
                                    </div>
                                )
                            } else {
                                return null
                            }
                        })
                    }
                    { walletLoading ? <Loader /> : null}
                    {
                        (errorToast?.display) ? (<>
                            <span className="error">{errorToast.message}</span>
                            <span className="link" style={{marginLeft: '5px'}} onClick={openConnectWalletModal}>Retry</span>
                        </>): null
                    }
                </div>
            </Modal>
        </>
    )
}

export default ConnectWallet;