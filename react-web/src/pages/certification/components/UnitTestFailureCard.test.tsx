import { fireEvent, render, screen } from "@testing-library/react";
import UnitTestFailureCard from "./UnitTestFailureCard";

describe("UnitTestFailureCard component", () => {
  const mockData = {
    resultObj: {
      resultShortDescription: "Unit test failed",
      resultDescription: "Test case failed due to an error",
    },
  };

  test("renders unit test failure card with failure information", () => {
    render(<UnitTestFailureCard {...mockData} />);

    expect(screen.getByText("Task: UnitTest")).toBeInTheDocument();
    expect(screen.getByText("Unit test failed")).toBeInTheDocument();
    expect(screen.getByText("Test case failed due to an error")).toBeInTheDocument();
  });

  test("toggles accordion when clicked", () => {
    render(<UnitTestFailureCard {...mockData} />);

    expect(screen.getByTestId("arrow")).toHaveClass("up");
    fireEvent.click(screen.getByTestId("accordion-title"));
    expect(screen.getByTestId("arrow")).toHaveClass("down");
  });
});