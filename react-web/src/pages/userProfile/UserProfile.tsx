import React, { useState, useEffect, useRef } from "react";
import { useForm } from "hooks/useForm";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useConfirm } from "material-ui-confirm";
import Button from "components/Button/Button";
import { Input } from "compositions/Form/components/Input";
import { Form } from "compositions/Form/Form";
import { fetchData } from "api/api";
import { getProfileDetails } from "store/slices/auth.slice";
import { useAppDispatch, useAppSelector } from "store/store";
import "./UserProfile.scss";
import { userProfileSchema } from "./userProfile.schema";
import { IUserProfile } from "./userProfile.interface";
import { clearStates,
  clearAccessToken,
  getUserAccessToken,
  verifyRepoAccess,
  hideConfirmConnection } from "./slices/repositoryAccess.slice";
import Toast from "components/Toast/Toast";
import useLocalStorage from "hooks/useLocalStorage";
import { LocalStorageKeys } from "constants/constants";

const UserProfile = () => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const confirm = useConfirm();
  const { userDetails, address, wallet, walletName } = useAppSelector((state: any) => state.auth);
  const [isEdit, setIsEdit] = useState(false);
  const { verifying, accessible, showConfirmConnection, accessToken } = useAppSelector((state) => state.repoAccess);
  const [owner, setOwner] = useState('');
  const [repo, setRepo] = useState('');
  const [timer, setTimer] = useState<any>(null);
  const [activeOwner, setActiveOwner] = useState(true);
  const [activeRepo, setActiveRepo] = useState(true);
  const [ownerErr, setOwnerErr] = useState(false);
  const [repoErr, setRepoErr] = useState(false);
  const [canShowConnectModal, setCanShowConnectModal] = useState(false)
  const [focussedOwnerRepo, setFocussedOwnerRepo] = useState(false)
  const [showError, setShowError] = useState("")

  const [searchParams, setSearchParams] = useSearchParams();
  const githubAccessCode = searchParams.get("code");

  const [, setUserDetails] = useLocalStorage(
    LocalStorageKeys.userDetails,
    localStorage.getItem(LocalStorageKeys.userDetails)
      ? JSON.parse(localStorage.getItem(LocalStorageKeys.userDetails)!)
      : null
  );

  const form: any = useForm({
    schema: userProfileSchema,
    mode: "onChange",
  });

  const initializeFormState = () => {
    form.clearErrors(); // clear form errors
    
    const { dapp, contacts, authors, linkedin, twitter, vendor, website } = userDetails;
    let formData: {
      contacts?: string;
      authors?: string;
      linkedin?: string;
      twitter?: string;
      vendor?: string;
      website?: string;
      name?: string;
      repo?: string;
      owner?: string;
      version?: string;
    } = { contacts, authors, linkedin, twitter, vendor, website };

    const profileInLS: any = localStorage.getItem(LocalStorageKeys.profile)
    if (profileInLS && profileInLS !== 'undefined') {
      const profileFormData = JSON.parse(profileInLS);
      setOwner(profileFormData.owner);
      setRepo(profileFormData.repo)
      form.reset(profileFormData)
    }
    else if (dapp !== null) {
      const { name, owner, repo, version } = dapp
      formData = { ...formData, name, version }
      setOwner(owner);
      setRepo(repo)
      form.reset(formData);
    }

    if (!userDetails.dapp?.owner || !userDetails.dapp?.repo || profileInLS) {
      setIsEdit(true);
    }
  }

  useEffect(() => {
    if (isEdit) {
      form.setFocus("name"); // focus on first field on Edit mode
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [isEdit]);

  useEffect(() => {
    initializeFormState()
    dispatch(clearStates())
    // initializeFormState() is to not to be triggered on every re-render of the dep-array below but whenever the form or userDetails is updated alone
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [userDetails, form, dispatch]);

  const clearOwnerRepoError = () => {
    setOwnerErr(false)
    setRepoErr(false)
  }

  const isOwnerRepoValidationError = () => {
    !owner.length ? setOwnerErr(true) : setOwnerErr(false);
    !repo.length ? setRepoErr(true) : setRepoErr(false);
    return !owner.length || !repo.length;
  }

  const formHandler = (formData: any) => {
    if (isOwnerRepoValidationError()) {
      return;
    }
    const {authors, contacts, linkedin, twitter, vendor, website} = formData
    const submitProfile = async () => {
      const reqData: IUserProfile = {
        "dapp": {
          "name": formData.name,
          "owner": owner,
          "repo": repo,
          "version": formData.version,
          "githubToken": accessToken || null
        }
      }
      if (authors) { reqData['authors'] = authors }
      if (contacts) { reqData['contacts'] = contacts }
      if (linkedin) { reqData['linkedin'] = linkedin }
      if (twitter) { reqData['twitter'] = twitter }
      if (vendor) { reqData['vendor'] = vendor }
      if (website) { reqData['website'] = website }

      fetchData.put("/profile/current", reqData).then(async () => {
      /** For mock */
      // await fetchData.get("static/data/current-profile.json", formData);
        const response = await dispatch(
          getProfileDetails({address: address, wallet: wallet, walletName: walletName})
          /** For mock */
          // getProfileDetails({url: "static/data/new-profile.json"})
        );
        setUserDetails(response.payload);
        dispatch(clearAccessToken())
        navigate('/')
      }).catch((errorObj) => {
        let errorMsg = 'Something went wrong. Please try again.'
        if (errorObj?.response?.data) {
          errorMsg = errorObj.response.statusText + ' - ' + errorObj.response.data
        }
        setShowError(errorMsg);
        const timeout = setTimeout(() => { clearTimeout(timeout); setShowError("") }, 5000)
        setUserDetails({ dapp: null });        
      })
    };
    submitProfile();
  };

  useEffect(() => {
    if (focussedOwnerRepo) {
      if (timer) {
        clearTimeout(timer)
        setTimer(null);
        dispatch(hideConfirmConnection());
      }
      const newTimer = setTimeout(() => {
        if (owner.length && repo.length) {
          clearOwnerRepoError()
          setCanShowConnectModal(true)
          dispatch(verifyRepoAccess({owner: owner, repo: repo}));
        } else {
          isOwnerRepoValidationError()
        }
      }, 600)
      setTimer(newTimer)
    }
  // the enclosed snippet is to be triggered only on update of owner,repo values and none else; this is a timer in effect to mimic the HTMLInput onChange mechanism
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [owner, repo])

  const inputChanged = (e: any,type:string) => {
    const currentVal = e.target.value
    if(type === 'owner') {
      setOwner(currentVal)
    } else if (type ==='repo') {
      setRepo(currentVal)
    }
    setCanShowConnectModal(false)
    setFocussedOwnerRepo(true)
    dispatch(hideConfirmConnection());
  }

  const inputBlurred = () => {
    setFocussedOwnerRepo(false)
    setCanShowConnectModal(false)
  }

  // Lists all repo and have to manually click 'Grant' for the repo we need.
  // If that is not chosen, it doesn't ask again for permission

  useEffect(() => {
    // to extract ?code= from the app base url redirected after Github connect
    if (githubAccessCode) {
      (async () => {
        await dispatch(getUserAccessToken({code: githubAccessCode}))
        searchParams.delete("code");
        setSearchParams(searchParams);
        const formData = form.getValues()
        setCanShowConnectModal(false)
        setFocussedOwnerRepo(false)
        const timeout = setTimeout(async () => {
          clearTimeout(timeout);
          setCanShowConnectModal(true)
          clearTimeout(timer)
          setTimer(null)
          await dispatch(verifyRepoAccess({owner: formData.owner, repo: formData.repo}))
          localStorage.removeItem(LocalStorageKeys.profile)
          localStorage.removeItem(LocalStorageKeys.accessToken)
        }, 0)
      })()
    } else {
      setCanShowConnectModal(false)
      localStorage.removeItem(LocalStorageKeys.profile)
    }
    // the enclosed snippet is to be triggered only once right when the component is rendered to check if the url contains code (to validate if it is a redirect from github)
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const connectToGithub = async () => {
    const data = {...form.getValues(), owner: owner, repo: repo}
    // store current form data in localStorage
    localStorage.setItem(LocalStorageKeys.profile, JSON.stringify(data))

    // fetch CLIENT_ID from api
    const clientId = (await fetchData.get("/github/client-id")).data as string
    window.location.assign(`https://github.com/login/oauth/authorize?client_id=${clientId}&scope=repo`)
  }


  const confirmConnectModal = () => {
    setTimeout(() => {
      confirm({
        title: "Verify the Repository details",
        description: "Unable to find the entered repository. Please go back and correct the Owner/Repository. Or, is this a Private one? If so, please hit Connect to authorize us to access it!",
        confirmationText: "Connect",
        cancellationText: "Go back"
      }).then( connectToGithub )
    }, 0)
  }

  const ownerRef = useRef<HTMLInputElement>(null);
  const repoRef = useRef<HTMLInputElement>(null);
  return (
    <>
      <div id="profileContainer">
        <div>
          <Form form={form} onSubmit={formHandler}>
            <Input
              label="dApp Name"
              type="text"
              id="name"
              required={true}
              disabled={!isEdit}
              {...form.register("name")}
            />
            <div className="input-wrapper">
              <div
                className={`input ${activeOwner ? "active" : ""} ${
                  !isEdit ? "disabled" : ""
                }`}
                onClick={(_) => {
                  setActiveOwner(true);
                  setFocussedOwnerRepo(true)
                  ownerRef.current?.focus();
                }}
              >
                <label>dApp Owner<span style={{color: 'red'}}>*</span></label>
                <input
                  id="owner"
                  ref={ownerRef}
                  value={owner}
                  type="text"
                  onChange={(e) => inputChanged(e, "owner")}
                  onBlur={inputBlurred}
                />
              </div>
              {ownerErr? <span className="danger error-msg">This field is required</span> : ''}
            </div>
            <label>
              {isEdit ? (
                <>
                  {verifying ? (
                    <label className="info">
                      <img
                        className="image anim-rotate"
                        data-testid="running"
                        src="images/running.svg"
                        alt="running"
                      />
                      <span style={{ color: "#DBAB0A" }}>Verifying</span>
                    </label>
                  ) : null}
                  {accessible && !verifying ? (
                    <label className="info">
                      <img
                        className="image"
                        data-testid="passed"
                        src="images/passed.svg"
                        alt="passed"
                      />
                      <span style={{ color: "#009168" }}>Accessible</span>
                    </label>
                  ) : showConfirmConnection ? (
                    <label
                      className="info"
                      onClick={confirmConnectModal}
                      style={{ cursor: "pointer" }}
                    >
                      <img
                        className="image"
                        data-testid="failed"
                        src="images/failed.svg"
                        alt="failed"
                      />
                      <span style={{ color: "#FF493D" }}>Not Accessible</span>
                    </label>
                  ) : null}
                </>
              ) : null}
            </label>
            <div className="input-wrapper">
              <div  className={`input ${activeRepo ? "active" : ""} ${
                  !isEdit ? "disabled" : ""
                }`}
                onClick={(_) => {
                  setActiveRepo(true);
                  setFocussedOwnerRepo(true)
                  repoRef.current?.focus();
                }}>
                <label>dApp Repository<span style={{color: 'red'}}>*</span></label>
                <input
                  id="repo"
                  ref={repoRef}
                  value={repo}
                  type="text"
                  onChange={(e) => inputChanged(e, "repo")}
                  onBlur={inputBlurred}
                />
              </div>
              {repoErr? <span className="danger error-msg">This field is required</span>: ''}
            </div>
            <Input
              label="dApp Version"
              type="text"
              id="version"
              required={true}
              disabled={!isEdit}
              {...form.register("version")}
            />
            <Input
              label="Authors"
              type="text"
              id="authors"
              disabled={!isEdit}
              {...form.register("authors")}
            />
            <Input
              label="Contacts"
              type="text"
              id="contacts"
              disabled={!isEdit}
              {...form.register("contacts")}
            />
            <Input
              label="Vendor"
              type="text"
              id="vendor"
              disabled={!isEdit}
              {...form.register("vendor")}
            />
            <Input
              label="Website"
              type="text"
              id="website"
              disabled={!isEdit}
              {...form.register("website")}
            />
            <Input
              label="LinkedIn Url"
              type="text"
              id="linkedIn"
              disabled={!isEdit}
              {...form.register("linkedin")}
            />
            <Input
              label="Twitter Url"
              type="text"
              id="twitter"
              disabled={!isEdit}
              {...form.register("twitter")}
            />
            <div className="button-wrapper">
              {!isEdit ? (
                <Button
                  type="button"
                  buttonLabel={"Edit"}
                  onClick={(e) => {
                    setIsEdit(!isEdit);
                  }}
                />
              ) : (
                <>
                  <Button
                    type="button"
                    displayStyle="secondary"
                    buttonLabel={"Cancel"}
                    onClick={() => {
                      localStorage.removeItem(LocalStorageKeys.profile)
                      initializeFormState()
                      setIsEdit(false);
                    }}
                  />
                  <Button
                    disabled={!form.formState.isValid || verifying || (!accessible && canShowConnectModal) || ownerErr || repoErr}
                    type="submit"
                    buttonLabel={"Save"}
                  />
                </>
              )}
            </div>
          </Form>
        </div>
      </div>
      {showError ? <Toast message={showError} /> : null}
      {showConfirmConnection && isEdit && canShowConnectModal ? confirmConnectModal() : null}
    </>
  );
};

export default UserProfile;
