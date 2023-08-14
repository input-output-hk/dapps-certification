import { fireEvent, render, screen } from "@testing-library/react";
import Button from "./Button";

describe("renders the Button component with defaults", () => {
  it("renders with default properties", () => {
    render(<Button />);
    const button = screen.getAllByRole("button")[0];
    expect(button).toHaveClass("btn");
    expect(button).toHaveAttribute("type", "submit");
    expect(button).toHaveClass("btn-medium");
    expect(button).toHaveClass("btn-primary");
    expect(button).toHaveTextContent("Submit");
    expect(button).not.toHaveClass("is-loading");
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
    render(<Button onClick={mockOnClick} />);
    const clickIndicator = screen.getByRole("button");
    fireEvent.click(clickIndicator);
    expect(mockOnClick).toHaveBeenCalledTimes(1);
  });
});