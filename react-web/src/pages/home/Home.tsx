import useLocalStorage from "hooks/useLocalStorage";
import Certification from "pages/certification/Certification";
import { LocalStorageKeys } from 'constants/constants';

const Home = () => {
  const [isLoggedIn] = useLocalStorage(
    LocalStorageKeys.isLoggedIn,
    localStorage.getItem(LocalStorageKeys.isLoggedIn) === "true" ? true : false
  );

  return isLoggedIn ? <Certification /> : <></>;
};

export default Home;