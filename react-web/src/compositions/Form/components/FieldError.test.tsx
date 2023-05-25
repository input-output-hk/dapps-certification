import { render, screen } from "@testing-library/react";
import { FieldError } from "./FieldError";

describe("renders FieldError component", () => {
  it("renders field error with default className", () => {
    render(<FieldError message="Sample"/>);
    expect(screen.getAllByRole("span")[0]).toHaveClass("danger");
  });

  it("renders field error with custom message", () => {
    render(<FieldError message="Sample"/>);
    expect(screen.getAllByRole("span")[0]).toHaveTextContent("Sample");
  });
});