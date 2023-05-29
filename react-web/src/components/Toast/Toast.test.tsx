import { render, screen } from "@testing-library/react";
import Toast from "./Toast";

describe("Toast component", () => {
  it("should render toast properly", () => {
    render(<Toast />);

    const toastWrapper = screen.getByTestId("toast");
    expect(toastWrapper).toBeInTheDocument();
  });

  it("should render toast properly with defaults", () => {
    render(<Toast />);

    const toastWrapper = screen.getByTestId("toast");
    expect(toastWrapper).toBeInTheDocument();

    const toastTitle = screen.queryByTestId("toast-title");
    expect(toastTitle).not.toBeInTheDocument();

    const toastMessage = screen.getByTestId("toast-message");
    expect(toastMessage.innerHTML).toBe(
      "Something wrong occurred. Please try again."
    );
  });

  it("should load toast with correct title", () => {
    render(<Toast title="Test title" />);
    const toastTitle = screen.getByTestId("toast-title");
    expect(toastTitle.innerHTML).toBe("Test title");
  });

  it("should load toast with correct message", () => {
    render(<Toast message="Test message" />);
    const toastMessage = screen.getByTestId("toast-message");
    expect(toastMessage.innerHTML).toBe("Test message");
  });
});