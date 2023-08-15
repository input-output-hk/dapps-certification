import React from "react";
import { render } from "@testing-library/react";
import ResultContainer from "./ResultContainer";

describe("ResultContainer component", () => {
  const mockData = {
    result: {
      _certRes_unitTestResults: [
        {
          resultDescription: "",
          resultOutcome: { tag: "Success" },
          resultShortDescription: "OK",
          resultTime: 0.6715505209999719,
        },
        {
          resultDescription: "",
          resultOutcome: { tag: "Success" },
          resultShortDescription: "OK",
          resultTime: 0.4284965800000009,
        },
        {
          resultDescription: "Script size: 5166\n",
          resultOutcome: { tag: "Success" },
          resultShortDescription: "OK",
          resultTime: 3.483856000002561e-3,
        },
      ],
      _certRes_DLTests: [
        [
          "redeem test",
          {
            classes: {
              "Contains Redeem": 100,
              "Contract instance for W[1] at endpoint pay-escrow": 100,
              "Contract instance for W[2] at endpoint pay-escrow": 100,
              "Contract instance for W[3] at endpoint pay-escrow": 100,
              "Contract instance for W[4] at endpoint redeem-escrow": 100,
              Redeemable: 100,
            },
            labels: [[[], 100]],
            numDiscarded: 0,
            numTests: 100,
            output:
              "+++ OK, passed 100 tests:\n100% Contains Redeem\n100% Contract instance for W[1] at endpoint pay-escrow\n100% Contract instance for W[2] at endpoint pay-escrow\n100% Contract instance for W[3] at endpoint pay-escrow\n100% Contract instance for W[4] at endpoint redeem-escrow\n100% Redeemable\n\nActions (400 in total):\n75.0% Pay\n25.0% Redeem\n",
            tables: { Actions: { Pay: 300, Redeem: 100 } },
            tag: "Success",
          },
        ],
        [
          "refund test",
          {
            classes: {
              "Contract instance for W[1] at endpoint pay-escrow": 100,
              "Contract instance for W[1] at endpoint refund-escrow": 100,
            },
            labels: [[[], 100]],
            numDiscarded: 0,
            numTests: 100,
            output:
              "+++ OK, passed 100 tests:\n100% Contract instance for W[1] at endpoint pay-escrow\n100% Contract instance for W[1] at endpoint refund-escrow\n\nActions (300 in total):\n33.3% Pay\n33.3% Refund\n33.3% WaitUntil\n\nWait interval (100 in total):\n100% 90-99\n\nWait until (100 in total):\n100% 100-199\n",
            tables: {
              Actions: { Pay: 100, Refund: 100, WaitUntil: 100 },
              "Wait interval": { "90-99": 100 },
              "Wait until": { "100-199": 100 },
            },
            tag: "Success",
          },
        ],
      ],
      _certRes_standardPropertyResult: {
        tag: "Failure",
        reason: "Reason 1",
        output: "Test output 5",
        failingTestCase: [
          "Actions \n [tok1 := SetupTokens,\n  tok4 := SetupTokens]",
          'InstanceErr (GenericError "failed to create currency")',
        ],
      },
      _certRes_doubleSatisfactionResult: [{ tag: "Success" }],
    },
  };

  test("renders unit test failures and successes", () => {
    const { queryByText } = render(<ResultContainer {...mockData} />);

    expect(queryByText(/\[tok1 := SetupTokens,/i)).toBeInTheDocument();
    expect(queryByText(/Task: StandardProperty/i)).toBeInTheDocument();
  });

  test("renders unit test failures", () => {
    const mockData = {
      result: {
        _certRes_unitTestResults: [
          {
            resultDescription:
              'Expected funds of W[1] to change by\n  Value (Map [(,Map [("",-100000000)])])\nbut they changed by\n  Value (Map [(,Map [("",-10000000)])])\na discrepancy of\n  Value (Map [(,Map [("",90000000)])])\nTest failed.\nEmulator log:\n[INFO] Slot 0: TxnValidate b45418acc3c57e0ce8e9f5426ad598dc726b929ded29efffcc40a1b0589ebca7\n[INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:\n                 Contract instance started\n[INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:\n                 Receive endpoint call on \'pay-escrow\' for Object (fromList [("contents",Array [Object (fromList [("getEndpointDescription",String "pay-escrow")]),Object (fromList [("unEndpointValue",Object (fromList [("getValue",Array [Array [Object (fromList [("unCurrencySymbol",String "")]),Array [Array [Object (fromList [("unTokenName",String "")]),Number 1.0e7]]]])]))])]),("tag",String "ExposeEndpointResp")])\n[INFO] Slot 1: W[1]: Balancing an unbalanced transaction:\n                       Tx:\n                         Tx 5b596c2af6164500abc5d321690c3203d1650e1e57766b3c89c0a35ecdccb23a:\n                           {inputs:\n                           reference inputs:\n                           collateral inputs:\n                           outputs:\n                             - Value (Map [(,Map [("",10000000)])]) addressed to\n                               ScriptCredential: 99ae93a7446e85fe4b6e7d2b58356c2b437fe39d8e6d7b7c8df8ebc6 (no staking credential)\n                               with datum hash 296d5467c4170fea176ba95ef0fcd17973b17d57ef8a94473f8856040a73fc23\n                               and with no reference script\n                           mint: Value (Map [])\n                           fee: Value (Map [])\n                           mps:\n                           signatures:\n                           validity range: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = -1596059091})) False, ivTo = UpperBound (Finite (Slot {getSlot = 40})) False}\n                           data:\n                             ( 296d5467c4170fea176ba95ef0fcd17973b17d57ef8a94473f8856040a73fc23\n                             , "\\162\\194\\fw\\136z\\206\\FS\\217\\134\\EM>Nu\\186\\189\\137\\147\\207\\213i\\149\\205\\\\\\252\\230\\t\\194" )}\n                       Requires signatures:\n                       Utxo index:\n[INFO] Slot 1: W[1]: Finished balancing:\n                       Tx fd491fdbc41aafc2332b14ad3e0b3bb615fc714f865d7ae8b745185c13841a87:\n                         {inputs:\n                            - b45418acc3c57e0ce8e9f5426ad598dc726b929ded29efffcc40a1b0589ebca7!50\n\n                         reference inputs:\n                         collateral inputs:\n                           - b45418acc3c57e0ce8e9f5426ad598dc726b929ded29efffcc40a1b0589ebca7!50\n\n                         outputs:\n                           - Value (Map [(,Map [("",10000000)])]) addressed to\n                             ScriptCredential: 99ae93a7446e85fe4b6e7d2b58356c2b437fe39d8e6d7b7c8df8ebc6 (no staking credential)\n                             with datum hash 296d5467c4170fea176ba95ef0fcd17973b17d57ef8a94473f8856040a73fc23\n                             and with no reference script\n                           - Value (Map [(,Map [("",9999999989823279)])]) addressed to\n                             PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)\n                             with no datum\n                             and with no reference script\n                         mint: Value (Map [])\n                         fee: Value (Map [(,Map [("",176721)])])\n                         validity range: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 0})) True, ivTo = UpperBound (Finite (Slot {getSlot = 40})) False}\n                         data:\n                           ( 296d5467c4170fea176ba95ef0fcd17973b17d57ef8a94473f8856040a73fc23\n                           , "\\162\\194\\fw\\136z\\206\\FS\\217\\134\\EM>Nu\\186\\189\\137\\147\\207\\213i\\149\\205\\\\\\252\\230\\t\\194" )\n                         redeemers:}\n[INFO] Slot 1: W[1]: Signing tx: fd491fdbc41aafc2332b14ad3e0b3bb615fc714f865d7ae8b745185c13841a87\n[INFO] Slot 1: W[1]: Submitting tx: fd491fdbc41aafc2332b14ad3e0b3bb615fc714f865d7ae8b745185c13841a87\n[INFO] Slot 1: W[1]: TxSubmit: fd491fdbc41aafc2332b14ad3e0b3bb615fc714f865d7ae8b745185c13841a87\n[INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:\n                 Contract instance stopped (no errors)\n[INFO] Slot 1: TxnValidate fd491fdbc41aafc2332b14ad3e0b3bb615fc714f865d7ae8b745185c13841a87\n  src/Plutus/Contract/Test.hs:312:\n  can pay\n',
            resultOutcome: {
              contents: { tag: "TestFailed" },
              tag: "Failure",
            },
            resultShortDescription: "FAIL",
            resultTime: 0.12417204799999126,
          },
          {
            resultDescription: "",
            resultOutcome: { tag: "Success" },
            resultShortDescription: "OK",
            resultTime: 0.6715505209999719,
          },
        ],
        _certRes_DLTests: [
          [
            "redeem test",
            {
              tag: "Failure",
              reason: "Reason 1",
              output: "Test output 5",
              failingTestCase: [
                'InstanceErr (GenericError "failed to create currency")',
              ],
            },
          ],
          [
            "refund test",
            {
              classes: {
                "Contract instance for W[1] at endpoint pay-escrow": 100,
                "Contract instance for W[1] at endpoint refund-escrow": 100,
              },
              labels: [[[], 100]],
              numDiscarded: 0,
              numTests: 100,
              output:
                "+++ OK, passed 100 tests:\n100% Contract instance for W[1] at endpoint pay-escrow\n100% Contract instance for W[1] at endpoint refund-escrow\n\nActions (300 in total):\n33.3% Pay\n33.3% Refund\n33.3% WaitUntil\n\nWait interval (100 in total):\n100% 90-99\n\nWait until (100 in total):\n100% 100-199\n",
              tables: {
                Actions: { Pay: 100, Refund: 100, WaitUntil: 100 },
                "Wait interval": { "90-99": 100 },
                "Wait until": { "100-199": 100 },
              },
              tag: "Success",
            },
          ],
        ],
      },
    };

    const { queryByTestId, queryByText } = render(
      <ResultContainer {...mockData} unitTestSuccess={false} />
    );

    expect(queryByTestId("failure-output")).toBeInTheDocument();
    expect(
      queryByText(/InstanceErr (GenericError "failed to create currency")/i)
    ).not.toBeInTheDocument();
  });
});
