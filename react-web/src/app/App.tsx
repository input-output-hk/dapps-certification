import React, { lazy, memo, Suspense } from "react";
import { Routes, Route, Outlet } from "react-router-dom";
import { BASE_URL } from "constants/route";
import Alert from '@mui/material/Alert';

import "./App.scss";
import Header from "components/Header/Header";
import PrivateRoutes from "components/PrivateRoutes/PrivateRoutes";
import NotFound from "components/NotFound/NotFound";
import Loader from "components/Loader/Loader";
import { useAppSelector } from "store/store";

const Certification = lazy(() => import("../pages/certification/Certification"));
const MaintenancePage = lazy(() => import("../pages/maintenance/Maintenance"));
const Community = lazy(() => import("../pages/community/Community"));
const TestHistory = lazy(() => import("../pages/testHistory/TestHistory"));
const UserProfile = lazy(() => import("../pages/userProfile/UserProfile"));
const Subscription = lazy(() => import("../pages/subscription/Subscription"));
const Support = lazy(() => import("../pages/support/Support"));
const Pricing = lazy(() => import("../pages/pricing/Pricing"));
const SubscriptionContent = lazy(() => import("../pages/subscription/SubscriptionContent"));
const Payment = lazy(() => import("../pages/subscription/payment/Payment"));
const Auditor = lazy(() => import("../pages/auditor/Auditor"));
const SubscriptionHistory = lazy(() => import("../pages/subscription/history/SubscriptionHistory"));


const PageLayout = () => {
  const { network } = useAppSelector((state) => state.auth);

  // const networkNames:{[x:string]:string} = {
  //   '0': 'Testnet',
  //   '1': 'Mainnet'
  // }

  const Banner = memo(() => {
    const networkEnvVar: any = process.env.REACT_APP_WALLET_NETWORK

    return (<>
      {network !== null && network !== 1 ? 
        // always show Info if not in Mainnet
        <Alert severity="info" style={{marginBottom: '10px'}}>Your connected wallet is not in Mainnet.</Alert> : null}
        {/* if not in Mainnet and app-wallet not Mainnet (i.e. in Testnet), show Warning to connect to Preprod. */}
      {network !== null && network !== 1 && networkEnvVar !== '1' ? 
        <Alert severity="warning">Being in a test network, please make sure you are connected to wallet in <strong>Preprod</strong> to avail services without any issues.</Alert> : null}
    </>)
  })

  return (
    <>
      <Header />
      <section id="globalBanners">
        <Banner />
      </section>
      {/* Load page content here */}
      <section data-testid="contentWrapper" id="contentWrapper">
        <Suspense fallback={<Loader />}>
          <Outlet />
        </Suspense>
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
            <Route path="/auditor" element={<Auditor />} />
            <Route path="/subscription" element={<Subscription />}>
              <Route path="" element={<SubscriptionContent/>} />  
              <Route path="payment" element={<Payment />} />
              <Route path="history" element={<SubscriptionHistory />} />
            </Route>
            <Route path="/history" element={<TestHistory />} />
            <Route path="/profile/*" element={<UserProfile />} />
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