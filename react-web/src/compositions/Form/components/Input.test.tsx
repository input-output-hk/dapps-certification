import { render, screen } from "@testing-library/react";
import { Input } from "./Input";

describe("renders Input component", () => {
  it("renders default Input component", () => {
    render(<Input label="Test" name="test"/>)
    expect(screen.getByTestId("test")).toBeInTheDocument();
  })
})