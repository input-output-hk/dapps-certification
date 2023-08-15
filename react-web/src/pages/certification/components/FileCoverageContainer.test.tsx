import React from "react";
import {
  act,
  fireEvent,
  render,
  screen,
  waitFor,
} from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import FileCoverageContainer from "./FileCoverageContainer";

const mockResult = {
  result: {
    _certRes_DLTests: [],
    _certRes_coverageReport: {
      _coverageData: {
        _coveredAnnotations: [
          {
            contents: {
              _covLocEndCol: 37,
              _covLocEndLine: 179,
              _covLocFile: "src/Plutus/Contracts/Uniswap/OnChain.hs",
              _covLocStartCol: 1,
              _covLocStartLine: 179,
            },
            tag: "CoverLocation",
          },
          {
            contents: {
              _covLocEndCol: 23,
              _covLocEndLine: 201,
              _covLocFile: "src/Plutus/Contracts/Uniswap/OnChain.hs",
              _covLocStartCol: 1,
              _covLocStartLine: 193,
            },
            tag: "CoverLocation",
          },
          {
            contents: [
              {
                _covLocEndCol: 108,
                _covLocEndLine: 212,
                _covLocFile: "src/Plutus/Contracts/Uniswap/OnChain.hs",
                _covLocStartCol: 42,
                _covLocStartLine: 212,
              },
              true,
            ],
            tag: "CoverBool",
          },
          {
            contents: [
              {
                _covLocEndCol: 57,
                _covLocEndLine: 218,
                _covLocFile: "src/Plutus/Contracts/Uniswap/OnChain.hs",
                _covLocStartCol: 24,
                _covLocStartLine: 218,
              },
              true,
            ],
            tag: "CoverBool",
          },
        ],
      },
      _coverageIndex: {
        _coverageMetadata: [
          [
            {
              contents: {
                _covLocEndCol: 20,
                _covLocEndLine: 268,
                _covLocFile: "src/Plutus/Contracts/Uniswap/OnChain.hs",
                _covLocStartCol: 5,
                _covLocStartLine: 268,
              },
              tag: "CoverLocation",
            },
            { _metadataSet: [] },
          ],
          [
            {
              contents: [
                {
                  _covLocEndCol: 30,
                  _covLocEndLine: 84,
                  _covLocFile: "src/Plutus/Contracts/Uniswap/OnChain.hs",
                  _covLocStartCol: 11,
                  _covLocStartLine: 84,
                },
                false,
              ],
              tag: "CoverBool",
            },
            {
              _metadataSet: [
                { contents: "ifThenElse", tag: "ApplicationHeadSymbol" },
              ],
            },
          ],
        ],
      },
    },
    _certRes_doubleSatisfactionResult: {
      classes: {
        "Contains Redeem": 65,
        "Contract instance for W[1] at endpoint badrefund-escrow": 37,
        "Contract instance for W[1] at endpoint pay-escrow": 55,
        "Contract instance for W[1] at endpoint redeem-escrow": 21,
        "Contract instance for W[1] at endpoint refund-escrow": 5,
        "Contract instance for W[2] at endpoint badrefund-escrow": 37,
        "Contract instance for W[2] at endpoint pay-escrow": 57,
        "Contract instance for W[2] at endpoint redeem-escrow": 30,
        "Contract instance for W[2] at endpoint refund-escrow": 12,
        "Contract instance for W[3] at endpoint badrefund-escrow": 38,
        "Contract instance for W[3] at endpoint pay-escrow": 52,
        "Contract instance for W[3] at endpoint redeem-escrow": 24,
        "Contract instance for W[3] at endpoint refund-escrow": 7,
        "Contract instance for W[4] at endpoint badrefund-escrow": 35,
        "Contract instance for W[4] at endpoint pay-escrow": 53,
        "Contract instance for W[4] at endpoint redeem-escrow": 23,
        "Contract instance for W[4] at endpoint refund-escrow": 6,
        "Contract instance for W[5] at endpoint badrefund-escrow": 39,
        "Contract instance for W[5] at endpoint pay-escrow": 55,
        "Contract instance for W[5] at endpoint redeem-escrow": 25,
        "Contract instance for W[5] at endpoint refund-escrow": 10,
        Redeemable: 78,
      },
      labels: [[[], 100]],
      numDiscarded: 11,
      numTests: 100,
      output:
        "+++ OK, passed 100 tests; 11 discarded:\n78% Redeemable\n65% Contains Redeem\n57% Contract instance for W[2] at endpoint pay-escrow\n55% Contract instance for W[1] at endpoint pay-escrow\n55% Contract instance for W[5] at endpoint pay-escrow\n53% Contract instance for W[4] at endpoint pay-escrow\n52% Contract instance for W[3] at endpoint pay-escrow\n39% Contract instance for W[5] at endpoint badrefund-escrow\n38% Contract instance for W[3] at endpoint badrefund-escrow\n37% Contract instance for W[1] at endpoint badrefund-escrow\n37% Contract instance for W[2] at endpoint badrefund-escrow\n35% Contract instance for W[4] at endpoint badrefund-escrow\n30% Contract instance for W[2] at endpoint redeem-escrow\n25% Contract instance for W[5] at endpoint redeem-escrow\n24% Contract instance for W[3] at endpoint redeem-escrow\n23% Contract instance for W[4] at endpoint redeem-escrow\n21% Contract instance for W[1] at endpoint redeem-escrow\n12% Contract instance for W[2] at endpoint refund-escrow\n10% Contract instance for W[5] at endpoint refund-escrow\n 7% Contract instance for W[3] at endpoint refund-escrow\n 6% Contract instance for W[4] at endpoint refund-escrow\n 5% Contract instance for W[1] at endpoint refund-escrow\n\nActions (2218 in total):\n50.72% BadRefund\n25.56% Pay\n15.10% WaitUntil\n 6.81% Redeem\n 1.80% Refund\n\nActions rejected by precondition (1112 in total):\n56.47% Redeem\n25.81% BadRefund\n15.11% Pay\n 2.61% Refund\n\nBad refund attempts (1125 in total):\n98.93% steal refund\n 1.07% early refund\n\nChainEvent type (9415 in total):\n79.17% SlotAdd\n11.72% TxnValidationFail\n 9.11% TxnValidate CardanoApiTx\n\nNumber of ChainEvents (100 in total):\n25% 10-19\n23% <10\n10% 100-199\n 7% 200-299\n 6% 40-49\n 5% 20-29\n 4% 30-39\n 3% 300-399\n 3% 50-59\n 3% 80-89\n 2% 400-499\n 2% 500-599\n 2% 70-79\n 2% 90-99\n 1% 60-69\n 1% 600-699\n 1% 900-999\n\nNumber of candidate counterexamples (100 in total):\n100% 0\n\nNumber of candidates to build counterexamples (100 in total):\n100% 0\n\nWait interval (335 in total):\n45.7% <10\n32.8% 10-19\n14.3% 20-29\n 6.3% 30-39\n 0.9% 40-49\n\nWait until (335 in total):\n19.1% 100-199\n14.0% 10-19\n 9.0% 200-299\n 8.7% <10\n 6.9% 30-39\n 6.6% 20-29\n 6.0% 300-399\n 6.0% 40-49\n 4.8% 50-59\n 3.9% 60-69\n 3.6% 400-499\n 3.6% 90-99\n 3.0% 70-79\n 2.1% 80-89\n 1.5% 500-599\n 0.9% 600-699\n 0.6% 700-799\n",
      tables: {
        Actions: {
          BadRefund: 1125,
          Pay: 567,
          Redeem: 151,
          Refund: 40,
          WaitUntil: 335,
        },
        "Actions rejected by precondition": {
          BadRefund: 287,
          Pay: 168,
          Redeem: 628,
          Refund: 29,
        },
        "Bad refund attempts": {
          "early refund": 12,
          "steal refund": 1113,
        },
        "ChainEvent type": {
          SlotAdd: 7454,
          "TxnValidate CardanoApiTx": 858,
          TxnValidationFail: 1103,
        },
        "Number of ChainEvents": {
          "10-19": 25,
          "100-199": 10,
          "20-29": 5,
          "200-299": 7,
          "30-39": 4,
          "300-399": 3,
          "40-49": 6,
          "400-499": 2,
          "50-59": 3,
          "500-599": 2,
          "60-69": 1,
          "600-699": 1,
          "70-79": 2,
          "80-89": 3,
          "90-99": 2,
          "900-999": 1,
          "<10": 23,
        },
        "Number of candidate counterexamples": {
          "0": 100,
        },
        "Number of candidates to build counterexamples": {
          "0": 100,
        },
        "Wait interval": {
          "10-19": 110,
          "20-29": 48,
          "30-39": 21,
          "40-49": 3,
          "<10": 153,
        },
        "Wait until": {
          "10-19": 47,
          "100-199": 64,
          "20-29": 22,
          "200-299": 30,
          "30-39": 23,
          "300-399": 20,
          "40-49": 20,
          "400-499": 12,
          "50-59": 16,
          "500-599": 5,
          "60-69": 13,
          "600-699": 3,
          "70-79": 10,
          "700-799": 2,
          "80-89": 7,
          "90-99": 12,
          "<10": 29,
        },
      },
      tag: "Success",
    },
    _certRes_noLockedFundsLightResult: null,
    _certRes_noLockedFundsResult: null,
    _certRes_standardCrashToleranceResult: null,
    _certRes_standardPropertyResult: {
      classes: {
        "Contains Redeem": 72,
        "Contract instance for W[1] at endpoint badrefund-escrow": 28,
        "Contract instance for W[1] at endpoint pay-escrow": 60,
        "Contract instance for W[1] at endpoint redeem-escrow": 30,
        "Contract instance for W[1] at endpoint refund-escrow": 4,
        "Contract instance for W[2] at endpoint badrefund-escrow": 28,
        "Contract instance for W[2] at endpoint pay-escrow": 61,
        "Contract instance for W[2] at endpoint redeem-escrow": 26,
        "Contract instance for W[2] at endpoint refund-escrow": 9,
        "Contract instance for W[3] at endpoint badrefund-escrow": 29,
        "Contract instance for W[3] at endpoint pay-escrow": 57,
        "Contract instance for W[3] at endpoint redeem-escrow": 23,
        "Contract instance for W[3] at endpoint refund-escrow": 4,
        "Contract instance for W[4] at endpoint badrefund-escrow": 34,
        "Contract instance for W[4] at endpoint pay-escrow": 57,
        "Contract instance for W[4] at endpoint redeem-escrow": 37,
        "Contract instance for W[4] at endpoint refund-escrow": 2,
        "Contract instance for W[5] at endpoint badrefund-escrow": 31,
        "Contract instance for W[5] at endpoint pay-escrow": 66,
        "Contract instance for W[5] at endpoint redeem-escrow": 27,
        "Contract instance for W[5] at endpoint refund-escrow": 8,
        Redeemable: 80,
      },
      labels: [[[], 100]],
      numDiscarded: 8,
      numTests: 100,
      output:
        "+++ OK, passed 100 tests; 8 discarded:\n80% Redeemable\n72% Contains Redeem\n66% Contract instance for W[5] at endpoint pay-escrow\n61% Contract instance for W[2] at endpoint pay-escrow\n60% Contract instance for W[1] at endpoint pay-escrow\n57% Contract instance for W[3] at endpoint pay-escrow\n57% Contract instance for W[4] at endpoint pay-escrow\n37% Contract instance for W[4] at endpoint redeem-escrow\n34% Contract instance for W[4] at endpoint badrefund-escrow\n31% Contract instance for W[5] at endpoint badrefund-escrow\n30% Contract instance for W[1] at endpoint redeem-escrow\n29% Contract instance for W[3] at endpoint badrefund-escrow\n28% Contract instance for W[1] at endpoint badrefund-escrow\n28% Contract instance for W[2] at endpoint badrefund-escrow\n27% Contract instance for W[5] at endpoint redeem-escrow\n26% Contract instance for W[2] at endpoint redeem-escrow\n23% Contract instance for W[3] at endpoint redeem-escrow\n 9% Contract instance for W[2] at endpoint refund-escrow\n 8% Contract instance for W[5] at endpoint refund-escrow\n 4% Contract instance for W[1] at endpoint refund-escrow\n 4% Contract instance for W[3] at endpoint refund-escrow\n 2% Contract instance for W[4] at endpoint refund-escrow\n\nActions (1730 in total):\n38.79% BadRefund\n34.97% Pay\n14.91% WaitUntil\n 9.77% Redeem\n 1.56% Refund\n\nActions rejected by precondition (768 in total):\n63.4% Redeem\n21.5% BadRefund\n11.1% Pay\n 4.0% Refund\n\nBad refund attempts (671 in total):\n97.5% steal refund\n 2.5% early refund\n\nWait interval (258 in total):\n46.1% <10\n33.3% 10-19\n14.7% 20-29\n 5.8% 30-39\n\nWait until (258 in total):\n14.0% 10-19\n14.0% <10\n12.8% 100-199\n10.5% 20-29\n 8.5% 40-49\n 8.1% 30-39\n 6.6% 200-299\n 5.0% 300-399\n 3.9% 50-59\n 3.9% 70-79\n 3.1% 60-69\n 2.7% 400-499\n 2.7% 90-99\n 2.3% 500-599\n 1.9% 80-89\n",
      tables: {
        Actions: {
          BadRefund: 671,
          Pay: 605,
          Redeem: 169,
          Refund: 27,
          WaitUntil: 258,
        },
        "Actions rejected by precondition": {
          BadRefund: 165,
          Pay: 85,
          Redeem: 487,
          Refund: 31,
        },
        "Bad refund attempts": {
          "early refund": 17,
          "steal refund": 654,
        },
        "Wait interval": {
          "10-19": 86,
          "20-29": 38,
          "30-39": 15,
          "<10": 119,
        },
        "Wait until": {
          "10-19": 36,
          "100-199": 33,
          "20-29": 27,
          "200-299": 17,
          "30-39": 21,
          "300-399": 13,
          "40-49": 22,
          "400-499": 7,
          "50-59": 10,
          "500-599": 6,
          "60-69": 8,
          "70-79": 10,
          "80-89": 5,
          "90-99": 7,
          "<10": 36,
        },
      },
      tag: "Success",
    },
    _certRes_unitTestResults: [],
    _certRes_whitelistOk: true,
    _certRes_whitelistResult: {
      classes: {
        "Contains Redeem": 73,
        "Contract instance for W[1] at endpoint badrefund-escrow": 40,
        "Contract instance for W[1] at endpoint pay-escrow": 63,
        "Contract instance for W[1] at endpoint redeem-escrow": 30,
        "Contract instance for W[1] at endpoint refund-escrow": 10,
        "Contract instance for W[2] at endpoint badrefund-escrow": 44,
        "Contract instance for W[2] at endpoint pay-escrow": 63,
        "Contract instance for W[2] at endpoint redeem-escrow": 31,
        "Contract instance for W[2] at endpoint refund-escrow": 14,
        "Contract instance for W[3] at endpoint badrefund-escrow": 39,
        "Contract instance for W[3] at endpoint pay-escrow": 64,
        "Contract instance for W[3] at endpoint redeem-escrow": 39,
        "Contract instance for W[3] at endpoint refund-escrow": 10,
        "Contract instance for W[4] at endpoint badrefund-escrow": 39,
        "Contract instance for W[4] at endpoint pay-escrow": 63,
        "Contract instance for W[4] at endpoint redeem-escrow": 22,
        "Contract instance for W[4] at endpoint refund-escrow": 17,
        "Contract instance for W[5] at endpoint badrefund-escrow": 49,
        "Contract instance for W[5] at endpoint pay-escrow": 60,
        "Contract instance for W[5] at endpoint redeem-escrow": 32,
        "Contract instance for W[5] at endpoint refund-escrow": 9,
        Redeemable: 81,
      },
      labels: [[[], 100]],
      numDiscarded: 18,
      numTests: 100,
      output:
        "+++ OK, passed 100 tests; 18 discarded:\n81% Redeemable\n73% Contains Redeem\n64% Contract instance for W[3] at endpoint pay-escrow\n63% Contract instance for W[1] at endpoint pay-escrow\n63% Contract instance for W[2] at endpoint pay-escrow\n63% Contract instance for W[4] at endpoint pay-escrow\n60% Contract instance for W[5] at endpoint pay-escrow\n49% Contract instance for W[5] at endpoint badrefund-escrow\n44% Contract instance for W[2] at endpoint badrefund-escrow\n40% Contract instance for W[1] at endpoint badrefund-escrow\n39% Contract instance for W[3] at endpoint badrefund-escrow\n39% Contract instance for W[3] at endpoint redeem-escrow\n39% Contract instance for W[4] at endpoint badrefund-escrow\n32% Contract instance for W[5] at endpoint redeem-escrow\n31% Contract instance for W[2] at endpoint redeem-escrow\n30% Contract instance for W[1] at endpoint redeem-escrow\n22% Contract instance for W[4] at endpoint redeem-escrow\n17% Contract instance for W[4] at endpoint refund-escrow\n14% Contract instance for W[2] at endpoint refund-escrow\n10% Contract instance for W[1] at endpoint refund-escrow\n10% Contract instance for W[3] at endpoint refund-escrow\n 9% Contract instance for W[5] at endpoint refund-escrow\n\nActions (2708 in total):\n50.41% BadRefund\n24.96% Pay\n15.47% WaitUntil\n 6.94% Redeem\n 2.22% Refund\n\nActions rejected by precondition (1227 in total):\n56.15% Redeem\n24.78% BadRefund\n15.73% Pay\n 3.34% Refund\n\nBad refund attempts (1365 in total):\n98.90% steal refund\n 1.10% early refund\n\nWait interval (419 in total):\n38.4% <10\n35.1% 10-19\n18.1% 20-29\n 7.9% 30-39\n 0.5% 40-49\n\nWait until (419 in total):\n16.5% 100-199\n12.2% 200-299\n11.9% 10-19\n 8.1% 20-29\n 8.1% 300-399\n 6.4% 40-49\n 5.7% 30-39\n 5.5% <10\n 5.3% 50-59\n 4.1% 60-69\n 3.6% 70-79\n 2.9% 400-499\n 2.9% 90-99\n 2.4% 500-599\n 1.9% 80-89\n 1.2% 700-799\n 1.0% 600-699\n 0.5% 800-899\n",
      tables: {
        Actions: {
          BadRefund: 1365,
          Pay: 676,
          Redeem: 188,
          Refund: 60,
          WaitUntil: 419,
        },
        "Actions rejected by precondition": {
          BadRefund: 304,
          Pay: 193,
          Redeem: 689,
          Refund: 41,
        },
        "Bad refund attempts": {
          "early refund": 15,
          "steal refund": 1350,
        },
        "Wait interval": {
          "10-19": 147,
          "20-29": 76,
          "30-39": 33,
          "40-49": 2,
          "<10": 161,
        },
        "Wait until": {
          "10-19": 50,
          "100-199": 69,
          "20-29": 34,
          "200-299": 51,
          "30-39": 24,
          "300-399": 34,
          "40-49": 27,
          "400-499": 12,
          "50-59": 22,
          "500-599": 10,
          "60-69": 17,
          "600-699": 4,
          "70-79": 15,
          "700-799": 5,
          "80-89": 8,
          "800-899": 2,
          "90-99": 12,
          "<10": 23,
        },
      },
      tag: "Success",
    },
  },
  githubLink: "https://github.com/Ali-Hill/plutus-apps/tree/a8de6d01bf95",
  coverageFile:
    '<body ><h1 >Files</h1><ul ><li ><a href="#src/Plutus/Contracts/Uniswap/OnChain.hs" >src/Plutus/Contracts/Uniswap/OnChain.hs</a></li></ul><hr><h2 id="src/Plutus/Contracts/Uniswap/OnChain.hs" >src/Plutus/Contracts/Uniswap/OnChain.hs</h2><pre ><div style=color:red >Source code not available</div>\n</pre></body>',
};

describe("FileCoverageContainer", () => {
  test("renders the component without crashing", () => {
    render(
      <FileCoverageContainer
        result={mockResult.result}
        githubLink={mockResult.githubLink}
        coverageFile={mockResult.coverageFile}
      />
    );
    // Verify that file links are rendered
    const linkElements = screen.queryAllByTestId("file-link");
    expect(linkElements.length).not.toBe(0);
  });

  test("renders the component without crashing and opens coverage file on click", async () => {
    render(
      <FileCoverageContainer
        result={mockResult.result}
        githubLink={mockResult.githubLink}
        coverageFile={mockResult.coverageFile}
      />
    );

    // Verify that file links are rendered
    const linkElements = screen.queryAllByTestId("file-link");
    fireEvent.click(linkElements[0]);

    expect(screen.getByRole("dialog")).toBeInTheDocument();
    expect(screen.getByText(/Source code not available/i)).toBeInTheDocument();

    // Modal close simulation
    const closeBtn = screen.getByRole("button");
    fireEvent.click(closeBtn);
    waitFor(() => expect(screen.getByRole("dialog")).not.toBeInTheDocument());
  });

  test("renders the component without coverage files", () => {
    render(
      <FileCoverageContainer result={{}} githubLink={mockResult.githubLink} />
    );
    // Verify that no file links are rendered
    const linkElements = screen.queryAllByTestId("file-link");
    expect(linkElements.length).toBe(0);
  });
});