import { fireEvent, render, screen } from "@testing-library/react";
import { Link, MemoryRouter, Outlet, Route, Routes } from "react-router-dom";
import "@testing-library/jest-dom";

import Support from "pages/support/Support";


const Header = () => {
  return (
    <>
      <li data-testid="support">
        <Link to="support">Support</Link>
      </li>
    </>
  );
};

describe("renders landing page", () => {
  // it("header contains button Connect Wallet", () => {})
})

describe("renders banners", () => {
  // it("renders banner when not in mainnet", () => {});
  // it("renders banner to ensure network is preprod", () => {});
});

describe("renders components on click of menu links", () => {
  it("header links navigates to corresponding components successfully", async () => {
    render(
      <MemoryRouter>
        <Routes>
          <Route
            path="/"
            element={
              <>
                <Header />
                <Outlet />
              </>
            }
          >
            <Route path="/support" element={<Support />} />
          </Route>
        </Routes>
      </MemoryRouter>
    );

    // Check whether Header is loaded correctly
    expect(screen.getByText(/Support/i)).toBeInTheDocument();

    const supportPageLink = screen.getByText(/Support/i);
    // Simulate route click
    fireEvent.click(supportPageLink);

    // Expect content of support page to be in the DOM
    expect(screen.getByText(/Coming soon.../i)).toBeInTheDocument();
  });
});