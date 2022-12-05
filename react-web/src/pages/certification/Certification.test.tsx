import { render, screen } from "@testing-library/react";
import Certification from "./Certification";

describe("Test cases for Form component", () => {
  test("should display correct error message", () => {
    render(<Certification />);
    screen.findByText("Enter Github repository details").then(res => { });
  });
});