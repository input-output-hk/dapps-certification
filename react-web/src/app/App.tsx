import React, { lazy, Suspense } from "react";
import { Routes, Route, Outlet } from "react-router-dom";
import { BASE_URL } from "constants/route";

import "./App.scss";
import Header from "components/Header/Header";
import Loader from "components/Loader/Loader";

const Certification = lazy(
  () => import("../pages/certification/Certification")
);

const PageLayout = () => {
  return (
    <>
      <Header />
      {/* Load page content here */}
      <section data-testid="contentWrapper" id="contentWrapper">
        <Outlet />
      </section>
    </>
  );
};

const App = () => {
  return (
    <Suspense fallback={<Loader />}>
      <Routes>
        <Route path={BASE_URL} element={<PageLayout />}>
          <Route path="/" element={<Certification />} />
        </Route>
      </Routes>
    </Suspense>
  );
};

export default App;