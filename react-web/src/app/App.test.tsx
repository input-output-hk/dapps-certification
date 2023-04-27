import { render } from "@testing-library/react";
import { BrowserRouter } from "react-router-dom";
import App from "./App";

describe("renders the landing page", () => {
  render(
    <BrowserRouter>
      <App />
    </BrowserRouter>
  );
});

describe("renders banners", () => {
  it("renders banner when not in mainnet", () => {

  })
  it("renders banner to ensure network is preprod", () => {
    
  })
})