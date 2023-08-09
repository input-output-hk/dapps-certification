import React from "react";
import { render, fireEvent, screen } from "@testing-library/react";
import FailedStatus from "./FailedStatus";

describe("FailedStatus component", () => {
  const mockData = {
    taskName: "Task 1",
    reason: "Reason 1",
    output: "Output 1",
    failingTestCase: ["TestCase 1", "TestCase 2"],
  };

  test("renders task name, reason, and output", () => {
    render(<FailedStatus {...mockData} />);

    expect(screen.getByText("Task: Task 1")).toBeInTheDocument();
    expect(screen.getByText("Reason 1")).toBeInTheDocument();
    expect(screen.getByText("Output 1")).toBeInTheDocument();
  });

  test("renders failing test cases", () => {
    render(<FailedStatus {...mockData} />);

    expect(screen.getByText("Failing TestCase(s):")).toBeInTheDocument();
    expect(screen.getByText("1. TestCase 1")).toBeInTheDocument();
    expect(screen.getByText("2. TestCase 2")).toBeInTheDocument();
  });

  test("does not render failing test case if data is empty", () => {
    render(
      <FailedStatus {...{ ...mockData, failingTestCase: [] }} />
    );

    expect(screen.queryAllByRole("log").length).toBe(0);
  });

  test("toggles accordion on click", () => {
    render(<FailedStatus {...mockData} />);

    expect(screen.getByTestId("arrow")).toHaveClass("up");
    fireEvent.click(screen.getByTestId("accordion-title"));
    expect(screen.getByTestId("arrow")).toHaveClass("down");
  });
});