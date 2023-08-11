import { useEffect, useState, memo, useCallback } from "react";
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
import ConnectWallet from "components/ConnectWallet/ConnectWallet";
import AvatarDropDown from "components/AvatarDropdown/AvatarDropdown";
import DropoverMenu from "components/DropoverMenu/DropoverMenu";
import { LocalStorageKeys } from 'constants/constants';

const Header = () => {
  const { isLoggedIn, address, wallet, network } = useAppSelector(
    (state) => state.auth
  );
  const dispatch = useAppDispatch();
  const [isActive, setIsActive] = useState(false);
  const [pollForAddress, setPollForAddress] = useState(false);
  const [pollForNetwork, setPollForNetwork] = useState(false);
  const [featureList, setFeatureList] = useState<String[]>([]);
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
          }).catch((e: any) => console.error('Failed to get network ID:', e));

          const features: any = await fetchData.get(
            "/profile/current/subscriptions/active-features"
          );
          if (features.error) {
            console.error('Failed to fetch active features:', features.error);
            return;
          }
          await dispatch(setSubscribedFeatures(features.data));
          setFeatureList((features.data === null || features.data === undefined) ? [] : features.data)
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
        const response = await wallet.getChangeAddress().catch((e: any) => console.error('Failed to get change address:', e));
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
      }).catch((e: any) => console.error('Failed to get network ID:', e));
    },
    1 * 1000,
    pollForNetwork
  )

  const hasCachedAddress = () => {
    return (!localStorage.getItem('address')?.length || !localStorage.getItem('walletName')?.length)
  }

  const ShowConnectWallet = memo(() => {
    return (<>
      {hasCachedAddress() ? <ConnectWallet /> : null}
    </>)
  })

  const ShowAvatarDropdown = memo(() => {
    return (<>
      {(address && wallet) ? <AvatarDropDown /> : null}
    </>)
  })

  const AuditMainMenu = () => <span className="link menu-link">Auditing</span>
  const AuditSubMenu = () => {
    return (<>
      <Link to="audit-report-upload">Report Upload</Link>
    </>)
  }

  const NoAuthMenu = memo(() => {
    return (
      <>
        <li>
          <Link to="community">Community</Link>
        </li>
        <li>
          <Link to="pricing">Pricing</Link>
        </li>
        <li>
          <Link to="support">Support</Link>
        </li>
        <li className="button-wrap">
          <ShowConnectWallet />
        </li>
      </>
    );
  });
  const AuthenticatedMenu = memo(() => {
    return (
      <>
        <li>
          <Link to="support">Support</Link>
        </li>        
        <li>
          <Link to="subscription">Subscription</Link>
        </li>
        {featureList.indexOf('l2-upload-report') !== -1 ? (
          <li>
            <DropoverMenu 
              name="auditing" 
              mainElm={<AuditMainMenu />}
              listItems={<AuditSubMenu />}></DropoverMenu>
            
            
          </li>
        ) : null}
        <li>
          <Link to="history">Test History</Link>
        </li>
        <li>
          <ShowAvatarDropdown />
        </li>
      </>
    );
  });

  const ProfileSection = useCallback(() => {
    return (
      <ul className={`menu ${isActive ? "active-ul" : ""}`}>
        {isLoggedIn ? <AuthenticatedMenu /> : <NoAuthMenu />}
      </ul>
    );
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [isActive, isLoggedIn, featureList]);

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

      <ProfileSection />
    </header>
  );
};

export default memo(Header);