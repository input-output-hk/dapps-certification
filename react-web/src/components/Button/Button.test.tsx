import { fireEvent, render, screen } from "@testing-library/react";
import Button from "./Button";

describe("renders the Button component with defaults", () => {
  it("renders without a class name", () => {
    render(<Button />);
    expect(screen.getAllByRole("button")[0]).toHaveClass("btn");
  });

  it("renders without a type", () => {
    render(<Button />);
    expect(screen.getAllByRole("button")[0]).toHaveAttribute("type", "submit");
  });
});

describe("renders the Button component", () => {
  it("renders with a button label", () => {
    render(<Button buttonLabel="Login" />);
    expect(screen.getAllByRole("button")[0]).toHaveTextContent("Login");
  });

  it("renders with a button with custom class name", () => {
    render(<Button className="button-class" />);
    expect(screen.getAllByRole("button")[0]).toHaveClass("button-class");
  });

  it("successful button click", () => {
    const mockOnClick = jest.fn();
    const { getByRole } = render(<Button onClick={mockOnClick()} />);
    const clickIndicator = getByRole("button");
    fireEvent.click(clickIndicator);
    expect(mockOnClick).toHaveBeenCalledTimes(1);
  });
});