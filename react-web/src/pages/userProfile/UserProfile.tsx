import React, { useState, useEffect } from "react";
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
import { clearStates, clearAccessToken, getUserAccessToken, verifyRepoAccess } from "./slices/repositoryAccess.slice";

const UserProfile = () => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const confirm = useConfirm();
  const { userDetails, address, wallet, walletName } = useAppSelector((state: any) => state.auth);
  const [isEdit, setIsEdit] = useState(false);

  const { verifying, accessible, showConfirmConnection, accessToken } = useAppSelector((state) => state.repoAccess);

  const [searchParams, setSearchParams] = useSearchParams();
  const githubAccessCode = searchParams.get("code");

  const form: any = useForm({
    schema: userProfileSchema,
    mode: "onChange",
  });

  const initializeFormState = () => {
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
    
    if (dapp !== null) {
      const { name, owner, repo, version } = dapp
      formData = { ...formData, name, owner, repo, version }
    }

    const profileInLS: any = localStorage.getItem('profile')
    if (profileInLS && profileInLS !== 'undefined') {
      form.reset(JSON.parse(profileInLS))
    } else {
      form.reset(formData)
    }
    
    if (!userDetails.dapp?.owner || !userDetails.dapp?.repo || profileInLS) {
      setIsEdit(true);
    }

    localStorage.removeItem('profile')
    localStorage.removeItem('accessToken')
  }

  useEffect(() => {
    initializeFormState()
    dispatch(clearStates())
  }, [userDetails, form]);

  const formHandler = (formData: any) => {
    const submitProfile = async () => {
      const reqData: IUserProfile = {
        "authors": formData.authors,
        "contacts": formData.contacts,
        "dapp": {
          "name": formData.name,
          "owner": formData.owner,
          "repo": formData.repo,
          "version": formData.version,
          "githubToken": accessToken || null
        },
        "linkedin": formData.linkedin,
        "twitter": formData.twitter,
        "vendor": formData.vendor,
        "website": formData.website
      }

      fetchData.put("/profile/current", reqData).then(async (res) => {
      /** For mock */
      // await fetchData.get("static/data/current-profile.json", formData);
        const response = await dispatch(
          getProfileDetails({address: address, wallet: wallet, walletName: walletName})
          /** For mock */
          // getProfileDetails({url: "static/data/new-profile.json"})
        );
        dispatch(clearAccessToken())
        navigate('/')
      })
    };
    submitProfile();
  };

  
  const CLIENT_ID = "10c45fe6a101a3abe3a0"; //of CertificationApp OAuthApp owned by CertGroup in anusree91
  const CLIENT_SECRET = "9fe0553a4727c47b434e732c3e258ba54c992051";
  // Lists all repo and have to manually click 'Grant' for the repo we need. 
  // If that is not chosen, it doesn't ask again for permission


  // const CLIENT_ID = "Iv1.78d108e12af627c4" // of CertTestApp GithubApp under CertGroup
  // const CLIENT_SECRET = "a77d34c1ffde5c4a7f0749fd7f969d6bbcda1ae7"

  useEffect(() => {
    // to extract ?code= from the app base url redirected after Github connect
    if (githubAccessCode) {
      (async () => {
        const params = "?client_id=" + CLIENT_ID + "&client_secret=" + CLIENT_SECRET + "&code=" + githubAccessCode
        const access_token = await dispatch(getUserAccessToken({queryParams: params}))
        searchParams.delete("code");
        setSearchParams(searchParams);
        dispatch(verifyRepoAccess({owner: "CertTestOrg1", repo: "plutus-apps-pvt"}));
      })()
    } else {
      localStorage.removeItem('profile')
    }
  }, [])

  const connectToGithub = () => {
    // store current form data in localStorage
    localStorage.setItem('profile', JSON.stringify(form.getValues()))

    // fetch CLIENT_ID from api 
    window.location.assign("https://github.com/login/oauth/authorize?client_id=" + CLIENT_ID + "&scope=repo")
  }

  const onGitDetailsChange = (event?: any, type?: string) => {
    const ownerVal = form.getValues("owner"),
      repoVal = form.getValues("repo");
    if ((ownerVal === 'CertTestOrg1' || ownerVal === 'CertTestOrg2') && (repoVal === 'CertTestRepoPvt1' || repoVal === 'CertTestRepoPub1' || repoVal === 'CertTestRepoPvt3' || repoVal === 'plutus-apps' || repoVal === 'plutus-apps-pvt')) {
      dispatch(verifyRepoAccess({owner: ownerVal, repo: repoVal}));
    }
  };

  const confirmConnectModal = () => {
    setTimeout(() => {
      confirm({ 
        title: "Verify the Repository details", 
        description: "Unable to find the entered repository. Please go back and correct the Owner/Repository. Or, is this a Private one? If so, please hit Connect to authorize us to access it!",
        confirmationText: "Connect",
        cancellationText: "Go back"
      }).then(async () => {
          connectToGithub()
        })
        .catch(() => { });
    }, 0)
  }

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
              disablefocus="true"
              {...form.register("name")}
            />
            <Input
              label="dApp Owner"
              disabled={!isEdit}
              type="text"
              required={true}
              disablefocus="true"
              {...form.register("owner", {
                onChange: (e: any) => {
                  onGitDetailsChange(e, "owner");
                },
              })}
            />
            <label>
              <Input
                label="dApp Repository"
                disabled={!isEdit}
                type="text"
                required={true}
                disablefocus="true"
                {...form.register("repo", {
                  onChange: (e: any) => {
                    onGitDetailsChange(e, "repo");
                  },
                })}
              />
              {isEdit ? (
                <>
                  {verifying ? (
                    <>
                      <label className="info">
                        <img
                          className="image anim-rotate"
                          data-testid="running"
                          src="images/running.svg"
                          alt="running"
                        />
                        <span style={{ color: "#DBAB0A" }}>Verifying</span>
                      </label>
                    </>
                  ) : null}
                  {accessible && !verifying ? (
                    <>
                      <label className="info">
                        <img
                          className="image"
                          data-testid="passed"
                          src="images/passed.svg"
                          alt="passed"
                        />
                        <span style={{ color: "#009168" }}>Accessible</span>
                      </label>
                    </>
                  ) : showConfirmConnection ? (
                    <>
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
                    </>
                  ) : null}
                </>
              ) : null}
            </label>
            <Input
              label="dApp Version"
              type="text"
              id="version"
              required={true}
              disabled={!isEdit}
              disablefocus="true"
              {...form.register("version")}
            />
            <Input
              label="Authors"
              type="text"
              id="authors"
              disabled={!isEdit}
              disablefocus="true"
              {...form.register("authors")}
            />
            <Input
              label="Contacts"
              type="text"
              id="contacts"
              disabled={!isEdit}
              disablefocus="true"
              {...form.register("contacts")}
            />
            <Input
              label="Vendor"
              type="text"
              id="vendor"
              disablefocus="true"
              disabled={!isEdit}
              {...form.register("vendor")}
            />
            <Input
              label="Website"
              type="text"
              id="website"
              disabled={!isEdit}
              disablefocus="true"
              {...form.register("website")}
            />
            <Input
              label="LinkedIn Url"
              type="text"
              id="linkedIn"
              disablefocus="true"
              disabled={!isEdit}
              {...form.register("linkedin")}
            />
            <Input
              label="Twitter Url"
              type="text"
              id="twitter"
              disabled={!isEdit}
              disablefocus="true"
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
                      localStorage.removeItem('profile')
                      initializeFormState()
                      setIsEdit(false);
                    }}
                  />
                  <Button
                    disabled={!form.formState.isValid || !accessible}
                    type="submit"
                    buttonLabel={"Save"}
                    onClick={() => {}}
                  />
                </>
              )}
            </div>
          </Form>
        </div>
      </div>
      {showConfirmConnection && isEdit ? confirmConnectModal() : null}
    </>
  );
};

export default UserProfile;
