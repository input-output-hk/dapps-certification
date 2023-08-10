import React, { memo, useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { Address } from "@emurgo/cardano-serialization-lib-browser";
import { useAppDispatch, useAppSelector } from "store/store";
import {
  logout,
  getProfileDetails,
  setNetwork,
  setSubscribedFeatures,
} from "store/slices/auth.slice";
import "./Header.scss";

import { useDelayedApi } from "hooks/useDelayedApi";
import { fetchData } from "api/api";
import useLocalStorage from "hooks/useLocalStorage";
import { AuthenticatedMenu, NoAuthMenu } from "./Menu";
import { LocalStorageKeys } from 'constants/constants';

const Header = () => {
  const { isLoggedIn, address, wallet, network } = useAppSelector(
    (state) => state.auth
  );
  const dispatch = useAppDispatch();
  const [isActive, setIsActive] = useState(false);
  const [pollForAddress, setPollForAddress] = useState(false);
  const [pollForNetwork, setPollForNetwork] = useState(false);
  const [isLogged, setIsLoggedIn] = useLocalStorage(
    LocalStorageKeys.isLoggedIn,
    localStorage.getItem(LocalStorageKeys.isLoggedIn) === "true" ? true : false
  );

  const [, setUserDetails] = useLocalStorage(
    LocalStorageKeys.userDetails,
    localStorage.getItem(LocalStorageKeys.userDetails)
      ? JSON.parse(localStorage.getItem(LocalStorageKeys.userDetails)!)
      : null
  );

  const [, setSubscriptions] = useLocalStorage(
    LocalStorageKeys.hasSubscriptions,
    localStorage.getItem(LocalStorageKeys.hasSubscriptions) === "true" ? true : false
  );

  useEffect(() => {
    // check if address, walletName is in localStorage - login user without having to connect to wallet again
    const addressCache = localStorage.getItem(LocalStorageKeys.address);
    const walletNameCache = localStorage.getItem(LocalStorageKeys.walletName);
    const authToken = localStorage.getItem(LocalStorageKeys.authToken);
    if (addressCache?.length && walletNameCache?.length && authToken?.length) {
      (async () => {
        try {
          const enabledWallet = await window.cardano[walletNameCache].enable();
          const response: any = await dispatch(
            getProfileDetails({
              address: addressCache,
              wallet: enabledWallet,
              walletName: walletNameCache,
            })
          );
          setUserDetails(response.payload);
          setIsLoggedIn(true);

          enabledWallet.getNetworkId().then(async (data: number) => {
            dispatch(setNetwork(data));
          });

          const features = await fetchData.get(
            "/profile/current/subscriptions/active-features"
          );
          await dispatch(setSubscribedFeatures(features.data));

          if (!features.data?.length) {
            setSubscriptions(false);
          } else setSubscriptions(true);
        } catch (e) {
          console.log(e);
        }
      })();
    }
    // eslint-disable-next-line
  }, [dispatch, isLogged]);

  // useEffect(() => {
  //   if (isLoggedIn) {
  //     navigate("/");
  //   }
  // // can't add navigate to the array as it would break internal navigations
  // // eslint-disable-next-line react-hooks/exhaustive-deps
  // }, [isLoggedIn]);

  useEffect(() => {
    setPollForAddress(wallet && address && isLoggedIn);
    setPollForNetwork(wallet && address && isLoggedIn && network !== null);
  }, [wallet, address, isLoggedIn, network]);

  const forceUserLogout = () => {
    // account/network has been changed. Force logout the user
    setPollForAddress(false);
    setPollForNetwork(false);
    dispatch(logout());
    setUserDetails({ dapp: null });
    setIsLoggedIn(false);
  };

  useDelayedApi(
    async () => {
      setPollForAddress(false);
      let newAddress = "";
      if (wallet) {
        const response = await wallet.getChangeAddress();
        newAddress = Address.from_bytes(
          Buffer.from(response, "hex")
        ).to_bech32();
      }
      if (newAddress && address !== newAddress) {
        forceUserLogout();
      } else {
        setPollForAddress(true);
      }
    },
    1 * 1000,
    pollForAddress
  );

  useDelayedApi(
    async () => {
      setPollForNetwork(false);
      wallet.getNetworkId().then((id: number) => {
        // Preview/Preprod/Testnet are all 0. Switching among them cannot be distinguished.
        // But, switching to-and-fro Mainnet is triggered
        if (id !== network) {
          forceUserLogout();
        } else {
          setPollForNetwork(true);
        }
      });
    },
    1 * 1000,
    pollForNetwork
  );

  return (
    <header className="header">
      <Link to="/" state={{ insideNavigation: true }}>
        <img
          src="/images/logo.png"
          alt="IOHK logo"
          style={{ width: "82px", padding: "10px" }}
        />
      </Link>

      <input
        className="menu-btn"
        type="checkbox"
        id="menu-btn"
        onChange={(e) => setIsActive(!isActive)}
      />
      <label className="menu-icon" htmlFor="menu-btn">
        <span className="navicon"></span>
      </label>

      {/* Profile section */}
      <ul className={`menu ${isActive ? "active-ul" : ""}`}>
        {isLoggedIn ? <AuthenticatedMenu /> : <NoAuthMenu />}
      </ul>
    </header>
  );
};

export default memo(Header);