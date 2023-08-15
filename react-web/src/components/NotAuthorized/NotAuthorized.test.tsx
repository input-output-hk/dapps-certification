import React from "react";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router-dom";
import NotAuthorized from "./NotAuthorized";

describe("NotAuthorized", () => {
  test("renders the component with the correct content", () => {
    render(
      <MemoryRouter>
        <NotAuthorized />
      </MemoryRouter>
    );

    // Verify the presence of the title
    const titleElement = screen.getByText(/403 - ACCESS DENIED/i);
    expect(titleElement).toBeInTheDocument();

    // Verify the presence of the subtitle
    const subtitleElement = screen.getByText(
      /Oops, You don't have permission/i
    );
    expect(subtitleElement).toBeInTheDocument();

    // Verify the presence of the "Go to homepage" link
    const linkElement = screen.getByRole("link", { name: /go to homepage/i });
    expect(linkElement).toBeInTheDocument();
    expect(linkElement).toHaveAttribute("href", "/");
  });
});