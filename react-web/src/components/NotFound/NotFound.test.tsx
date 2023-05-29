import { fireEvent, render, screen } from "@testing-library/react";
import NotFound from "./NotFound";
import { MemoryRouter, Route, Routes } from "react-router";

describe("Not found component", () => {
  it("renders correctly", () => {
    render(
      <MemoryRouter>
        <NotFound />
      </MemoryRouter>
    );
    expect(screen.getByText("404 - No page found")).toBeInTheDocument();
  });

  it("renders component on accessing bad route", () => {
    render(
      <MemoryRouter initialEntries={["/test-path"]}>
        <Routes>
          <Route path="/" element={<div data-testid="home">Home</div>} />
          <Route path="*" element={<NotFound />} />
        </Routes>
      </MemoryRouter>
    );

    expect(screen.getByText("404 - No page found")).toBeInTheDocument();
  });
});