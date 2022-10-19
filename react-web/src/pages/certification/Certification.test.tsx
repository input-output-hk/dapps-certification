import { fireEvent, getByRole, render, screen } from "@testing-library/react";
import Certification from "./Certification";

describe("Test cases for Form component", () => {
  test("should display correct error message", () => {
    const { findByText, getByRole } = render(<Certification />);
    fireEvent.click(getByRole("button"));
    findByText("This field is required");
  });
});