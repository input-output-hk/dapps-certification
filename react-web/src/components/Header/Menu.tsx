import { memo } from "react";
import { Link } from "react-router-dom";
import { useAppSelector } from "store/store";

import ConnectWallet from "components/ConnectWallet/ConnectWallet";
import AvatarDropDown from "components/AvatarDropdown/AvatarDropdown";
import { LocalStorageKeys } from "constants/constants";

export const NoAuthMenu = memo(() => {
  const hasCachedAddress =
    !localStorage.getItem(LocalStorageKeys.address)?.length ||
    !localStorage.getItem(LocalStorageKeys.walletName)?.length;

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
        <>{hasCachedAddress ? <ConnectWallet /> : null}</>
      </li>
    </>
  );
});

export const AuthenticatedMenu = memo(() => {
  const { address, wallet, subscribedFeatures } = useAppSelector(
    (state) => state.auth
  );

  return (
    <>
      <li>
        <Link to="support">Support</Link>
      </li>
      {subscribedFeatures &&
      subscribedFeatures?.indexOf("l2-upload-report") !== -1 ? (
        <li>
          <Link to="auditor">Auditor</Link>
        </li>
      ) : null}
      <li>
        <Link to="subscription">Subscription</Link>
      </li>
      <li>
        <Link to="history">Test History</Link>
      </li>
      <li>
        <>{address && wallet ? <AvatarDropDown /> : null}</>
      </li>
    </>
  );
});