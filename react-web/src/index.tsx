import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter } from "react-router-dom";
import { ConfirmProvider } from "material-ui-confirm";
import App from "./app/App";
import "./index.scss";
import reportWebVitals from "./reportWebVitals";

import { Provider } from "react-redux";
import store from "store/store";

const root = ReactDOM.createRoot(
  document.getElementById("root") as HTMLElement
);
root.render(
  <BrowserRouter basename={process.env.PUBLIC_URL}>
    <ConfirmProvider>
      <Provider store={store}>
        <App />
      </Provider>
    </ConfirmProvider>
  </BrowserRouter>
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
