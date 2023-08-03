import useLocalStorage from "hooks/useLocalStorage";
import Certification from "pages/certification/Certification";

const Home = () => {
  const [isLoggedIn] = useLocalStorage(
    "isLoggedIn",
    localStorage.getItem("isLoggedIn") === "true" ? true : false
  );

  return isLoggedIn ? <Certification /> : <></>;
};

export default Home;