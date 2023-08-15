import { render } from "@testing-library/react";
import { ReactNode } from "react";
import { Provider } from "react-redux";
import { BrowserRouter } from "react-router-dom";
import store from "store/store";

export const renderWithStoreAndRouter = (comp: ReactNode) => {
  const data = render(
    <BrowserRouter basename={process.env.PUBLIC_URL}>
      <Provider store={store}>{comp}</Provider>
    </BrowserRouter>
  );
  return data;
};