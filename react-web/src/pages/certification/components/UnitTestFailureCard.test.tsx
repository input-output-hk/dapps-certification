import React from "react";
import { fireEvent, render } from "@testing-library/react";
import UnitTestFailureCard from "./UnitTestFailureCard";

describe("UnitTestFailureCard component", () => {
  const mockData = {
    resultObj: {
      resultShortDescription: "Unit test failed",
      resultDescription: "Test case failed due to an error",
    },
  };

  test("renders unit test failure card with failure information", () => {
    const { getByText } = render(<UnitTestFailureCard {...mockData} />);

    expect(getByText("Task: UnitTest")).toBeInTheDocument();
    expect(getByText("Unit test failed")).toBeInTheDocument();
    expect(getByText("Test case failed due to an error")).toBeInTheDocument();
  });

  test("toggles accordion when clicked", () => {
    const { getByTestId } = render(<UnitTestFailureCard {...mockData} />);

    expect(getByTestId("arrow")).toHaveClass("up");
    fireEvent.click(getByTestId("accordion-title"));
    expect(getByTestId("arrow")).toHaveClass("down");
  });
});