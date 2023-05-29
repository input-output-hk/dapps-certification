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

  it("renders without a size", () => {
    render(<Button />);
    expect(screen.getAllByRole("button")[0]).toHaveClass("btn-medium");
  });

  it("renders without a displayStyle", () => {
    render(<Button />);
    expect(screen.getAllByRole("button")[0]).toHaveClass("btn-primary");
  });

  it("renders without a label", () => {
    render(<Button />);
    expect(screen.getAllByRole("button")[0]).toHaveTextContent("Submit");
  });

  it("renders without class is-loading", () => {
    render(<Button />);
    expect(screen.getAllByRole("button")[0]).not.toHaveClass("is-loading");
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

  it("renders a loading indicator with a showLoader", () => {
    render(<Button showLoader={true} />);
    expect(screen.getByTestId("spinner")).toBeInTheDocument()
  })

  it("renders with class is-loading when isLoading", () => {
    render(<Button isLoading={true} />);
    expect(screen.getAllByRole("button")[0]).toHaveClass("is-loading");
  })

  it("renders image when passed in iconUrl", () => {
    render(<Button buttonLabel="Download" iconUrl={"images/download.svg"} />);
    expect(screen.getAllByRole("button")[0]).toHaveTextContent("Download");
    expect(screen.getByTestId("btn-icon")).toBeInTheDocument();
    expect(screen.getByTestId("btn-icon")).toHaveAttribute("src", "images/download.svg")
  })

  it("successful button click", () => {
    const mockOnClick = jest.fn();
    render(<Button onClick={mockOnClick()} />);
    const clickIndicator = screen.getByRole("button");
    fireEvent.click(clickIndicator);
    expect(mockOnClick).toHaveBeenCalledTimes(1);
  });
});