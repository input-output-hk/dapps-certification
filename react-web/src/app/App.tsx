import React, { lazy, Suspense } from "react";
import { Routes, Route, Outlet } from "react-router-dom";
import { BASE_URL } from "constants/route";

import "./App.scss";
import Header from "components/Header/Header";
import PrivateRoutes from "components/PrivateRoutes/PrivateRoutes";
import NotFound from "components/NotFound/NotFound";
import Loader from "components/Loader/Loader";

const Certification = lazy(() => import("../pages/certification/Certification"));
const MaintenancePage = lazy(() => import("../pages/maintenance/Maintenance"));
const Community = lazy(() => import("../pages/community/Community"));
const TestHistory = lazy(() => import("../pages/testHistory/TestHistory"));
const UserProfile = lazy(() => import("../pages/userProfile/UserProfile"));
const Subscription = lazy(() => import("../pages/subscription/Subscription"));
const Support = lazy(() => import("../pages/support/Support"));
const Pricing = lazy(() => import("../pages/pricing/Pricing"));


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
          <Route element={<PrivateRoutes />}>
            <Route path="/" element={<Certification />} />
            <Route path="/subscription" element={<Subscription />} />
            <Route path="/test" element={<TestHistory />} />
            <Route path="/profile" element={<UserProfile />} />
          </Route>
          <Route path="/" element={<MaintenancePage />} />
          <Route path="/community" element={<Community />} />
          <Route path="/support" element={<Support />} />
          <Route path="/pricing" element={<Pricing />} />
        </Route>
        <Route path="*" element={<NotFound />} />
      </Routes>
    </Suspense>
  );
};

export default App;