import React from "react";
import { render, fireEvent } from "@testing-library/react";
import FailedStatus from "./FailedStatus";

describe("FailedStatus component", () => {
  const mockData = {
    taskName: "Task 1",
    reason: "Reason 1",
    output: "Output 1",
    failingTestCase: ["TestCase 1", "TestCase 2"],
  };

  test("renders task name, reason, and output", () => {
    const { getByText } = render(<FailedStatus {...mockData} />);

    expect(getByText("Task: Task 1")).toBeInTheDocument();
    expect(getByText("Reason 1")).toBeInTheDocument();
    expect(getByText("Output 1")).toBeInTheDocument();
  });

  test("renders failing test cases", () => {
    const { getByText } = render(<FailedStatus {...mockData} />);

    expect(getByText("Failing TestCase(s):")).toBeInTheDocument();
    expect(getByText("1. TestCase 1")).toBeInTheDocument();
    expect(getByText("2. TestCase 2")).toBeInTheDocument();
  });

  test("does not render failing test case if data is empty", () => {
    const { queryAllByRole } = render(
      <FailedStatus {...{ ...mockData, failingTestCase: [] }} />
    );

    expect(queryAllByRole("log").length).toBe(0);
  });

  test("toggles accordion on click", () => {
    const { getByTestId } = render(<FailedStatus {...mockData} />);

    expect(getByTestId("arrow")).toHaveClass("up");
    fireEvent.click(getByTestId("accordion-title"));
    expect(getByTestId("arrow")).toHaveClass("down");
  });
});