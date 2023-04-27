import { render } from "@testing-library/react";
import { Input } from "./Input";

describe("Test cases for FieldError component", () => {
  it("renders field error with default className", () => {
    const { container } = render(<Input label="test" name="test"/>);
    expect(container.firstChild).toHaveClass("danger");
  });

  it("renders field error with custom message", () => {
    const { container } = render(<Input label="test" name="test"/>);
    expect(container.firstChild).toHaveTextContent("Sample");
  });
});