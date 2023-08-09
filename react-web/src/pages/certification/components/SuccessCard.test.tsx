import React from "react";
import { render, fireEvent, screen } from "@testing-library/react";
import SuccessCard from "./SuccessCard";

describe("SuccessCard component", () => {
  test("renders success card with test results", () => {
    const mockData = {
      resultObj: {
        numTests: 10,
        numDiscarded: 2,
        tables: {
          data1: {},
          data2: {},
        },
      },
      taskName: "Task 1",
      unitTest: null,
    };
    render(<SuccessCard {...mockData} />);

    expect(screen.getByText(/Task: Task 1/)).toBeInTheDocument();
    expect(screen.getByText(/2 discarded/)).toBeInTheDocument();
    expect(screen.getByText(/OK, passed 10 tests/)).toBeInTheDocument();
  });

  test("renders success card with test results when unit test is not supplied", () => {
    const mockData = {
      resultObj: {
        numTests: 10,
        numDiscarded: 2,
        tables: {
          data1: {},
          data2: {},
        },
      },
      taskName: "Task 1",
    };
    render(<SuccessCard {...mockData} />);

    expect(screen.getByText(/Task: Task 1/)).toBeInTheDocument();
    expect(screen.getByText(/2 discarded/)).toBeInTheDocument();
    expect(screen.getByText(/OK, passed 10 tests/)).toBeInTheDocument();
  });

  test("toggles accordion when clicked", () => {
    const mockData = {
      resultObj: {
        numTests: 10,
        numDiscarded: 2,
        tables: {
          data1: {},
          data2: {},
        },
      },
      taskName: "Task 1",
      unitTest: null,
    };
    const { container } = render(<SuccessCard {...mockData} />);

    const accordionTitle = screen.getByText("OK, passed 10 tests");
    const accordionContent = container.querySelector(".accordion-content");

    // Initially, accordion content is visible
    expect(accordionContent).toHaveClass("accordion-content");

    // Clicking the accordion title toggles the accordion content
    fireEvent.click(accordionTitle);
    expect(accordionContent).toHaveClass("hidden");

    fireEvent.click(accordionTitle);
    expect(accordionContent).toHaveClass("accordion-content");
  });

  test("does not render discarded section when no. of tests discarded has negative value", () => {
    const mockData = {
      resultObj: {
        numTests: 10,
        numDiscarded: -1,
        tables: {
          data1: {},
          data2: {},
        },
      },
      taskName: "Task 1",
      unitTest: null,
    };
    render(<SuccessCard {...mockData} />);
    expect(screen.queryByText(/discarded/)).not.toBeInTheDocument();
  });

  test("renders successfully without tables", () => {
    const mockData = {
      resultObj: {
        numTests: 10,
        numDiscarded: 2,
      },
      taskName: "Task 1",
      unitTest: null,
    };
    render(<SuccessCard {...mockData} />);

    expect(screen.getByText(/Task: Task 1/)).toBeInTheDocument();
    expect(screen.getByText(/2 discarded/)).toBeInTheDocument();
    expect(screen.getByText(/OK, passed 10 tests/)).toBeInTheDocument();
  });

  test("renders successfully with unit test=_certRes_unitTestResults", () => {
    const mockData = {
      resultObj: [
        {
          resultDescription: "",
          resultOutcome: { tag: "Success" },
          resultShortDescription: "OK",
          resultTime: 0.59311624999998,
        },
        {
          resultDescription: "",
          resultOutcome: { tag: "Success" },
          resultShortDescription: "OK",
          resultTime: 0.6715505209999719,
        },
      ],
      taskName: "Task 1",
      unitTest: "_certRes_unitTestResults",
    };
    render(<SuccessCard {...mockData} />);

    expect(screen.getByText(/Task: Task 1/)).toBeInTheDocument();
    expect(screen.getByText(/OK, passed 2\/2 tests/)).toBeInTheDocument();
  });

  test("renders successfully with unit test=_certRes_DLTests", () => {
    const mockData = {
      resultObj: [
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
      ],
      taskName: "Task 1",
      unitTest: "_certRes_DLTests",
    };
    render(<SuccessCard {...mockData} />);

    expect(screen.getByText(/Task: Task 1/)).toBeInTheDocument();
    expect(screen.getByText(/OK, passed 1\/1 tests/)).toBeInTheDocument();

    // Collapse accordion data
    const accordionHeader = screen.getByTestId("_certRes_DLTests");
    fireEvent.click(accordionHeader);
    expect(screen.getByTestId("_certRes_DLTests-arrow")).toHaveClass("down", {
      exact: false,
    });
  });

  test("renders successfully with unit test=_certRes_DLTests when no.of tests discarded has negative value", () => {
    const mockData = {
      resultObj: [
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
            numDiscarded: -1,
            numTests: 100,
            output:
              "+++ OK, passed 100 tests:\n100% Contains Redeem\n100% Contract instance for W[1] at endpoint pay-escrow\n100% Contract instance for W[2] at endpoint pay-escrow\n100% Contract instance for W[3] at endpoint pay-escrow\n100% Contract instance for W[4] at endpoint redeem-escrow\n100% Redeemable\n\nActions (400 in total):\n75.0% Pay\n25.0% Redeem\n",
            tables: { Actions: { Pay: 300, Redeem: 100 } },
            tag: "Success",
          },
        ],
      ],
      taskName: "Task 1",
      unitTest: "_certRes_DLTests",
    };

    render(<SuccessCard {...mockData} />);
    expect(screen.queryByText(/discarded/)).not.toBeInTheDocument();
  });

  test("renders successfully with unit test is not _certRes_unitTestResults or _certRes_DLTests", () => {
    const mockData = {
      resultObj: [],
      taskName: "Task 1",
      unitTest: "some_random_type",
    };

    render(
      <SuccessCard {...mockData} />
    );
    expect(screen.getByText(/Task: Task 1/)).toBeInTheDocument();
    expect(screen.queryByTestId("_certRes_DLTests")).not.toBeInTheDocument();
    expect(screen.queryByTestId("_certRes_unitTestResults")).not.toBeInTheDocument();
  });
});