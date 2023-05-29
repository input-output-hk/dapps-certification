import { render, screen } from "@testing-library/react";
import Loader from "./Loader";

describe("Loader component", () => {
  it("renders loader properly", () => {
    render(<Loader />);
    expect(screen.getByTestId("spinner")).toBeInTheDocument();
  });
});