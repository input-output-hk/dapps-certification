import { render, screen } from "@testing-library/react";
import ButtonLoader from "./ButtonLoader";

describe("renders the ButtonLoader component", () => {
  it("renders on screen", () => {
    render(<ButtonLoader />);
    expect(screen.getByTestId("loader")).toBeInTheDocument();
  });

  it("renders without a default class name", () => {
    render(<ButtonLoader />);
    expect(screen.getByTestId("loader")).toHaveClass("spinner");
  });
});
