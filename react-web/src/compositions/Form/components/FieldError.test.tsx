import { render } from "@testing-library/react";
import { FieldError } from "./FieldError";

describe("Test cases for FieldError component", () => {
  it("renders field error with default className", () => {
    const { container } = render(<FieldError message="Sample" />);
    expect(container.firstChild).toHaveClass("danger");
  });

  it("renders field error with custom message", () => {
    const { container } = render(<FieldError message="Sample" />);
    expect(container.firstChild).toHaveTextContent("Sample");
  });
});