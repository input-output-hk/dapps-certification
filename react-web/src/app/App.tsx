import React from "react";
import { Routes, Route, Outlet } from "react-router-dom";
import { BASE_URL } from "constants/route";

import "./App.scss";
import Certification from "pages/certification/Certification";

const PageLayout = () => {
  return (
    <>
      <header></header>

      {/* Load page content here */}
      <section id="contentWrapper">
        <Outlet />
      </section>
    </>
  );
};

const App = () => {
  return (
    <Routes>
      <Route path={BASE_URL} element={<PageLayout />}>
        <Route index element={<Certification />}></Route>
      </Route>
    </Routes>
  );
};

export default App;
