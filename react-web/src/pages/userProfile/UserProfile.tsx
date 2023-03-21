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
import { getUserAccessToken, verifyRepoAccess } from "./slices/repositoryAccess.slice";

const UserProfile = () => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const confirm = useConfirm();
  const { userDetails, address, wallet, walletName } = useAppSelector((state: any) => state.auth);
  const [isEdit, setIsEdit] = useState(false);

  const { verifying, accessible, showConfirmConnection } = useAppSelector((state) => state.repoAccess);
  
  const form: any = useForm({
    schema: userProfileSchema,
    mode: "onChange",
  });

  useEffect(() => {
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
      // setOwnerVal(owner)
      // setRepoVal(repo)
    }

    form.reset(formData)
    
    if (!userDetails.dapp?.owner || !userDetails.dapp?.repo) {
      setIsEdit(true);
    }
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
          "githubToken": null
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
        navigate('/')
      })
    };
    submitProfile();
  };

  
  const CLIENT_ID = "10c45fe6a101a3abe3a0"; //of CertificationApp OAuthApp owned by CertGroup in anusree91
  const CLIENT_SECRET = "9fe0553a4727c47b434e732c3e258ba54c992051";

  // const CLIENT_ID = "Iv1.78d108e12af627c4" // of CertTestApp GithubApp under CertGroup
  // const CLIENT_SECRET = "a77d34c1ffde5c4a7f0749fd7f969d6bbcda1ae7"

  const [searchParams] = useSearchParams();
  useEffect(() => {
    // to extract ?code= from the app base url redirected after Github connect
    // const queryString = window.location.search; 
    // const urlParams = new URLSearchParams(queryString)
    // const code = urlParams.get('code')
    const code = searchParams.get("code");
    console.log(code) //afb8d4a70a5fb4c1c062
    if (code) {
      // send codeParam to api and get back access_token to be saved in store
      const params = "?client_id=" + CLIENT_ID + "&client_secret=" + CLIENT_SECRET + "&code=" + code
      dispatch(getUserAccessToken({queryParams: params}));
      onGitDetailsChange() // verifyRepo()
    }

  }, [])

  const connectToGithub = () => {
    // fetch CLIENT_ID from api 
    window.location.assign("https://github.com/login/oauth/authorize?client_id=" + CLIENT_ID + "&scope=repo")
  }

  // const [ownerVal, setOwnerVal] = useState("")
  // const [repoVal, setRepoVal] = useState("")
  
  // const onOwnerChange = (e:any) => {
  //   setOwnerVal(e.target.value)
  // }

  // const onRepoChange = (e:any) => {
  //   setRepoVal(e.target.value)
  // }

  // const verifyRepo = () => {
  //   console.log(repoVal, ownerVal)
  //   dispatch(verifyRepoAccess({owner: ownerVal, repo: repoVal}))
  // }

  const onGitDetailsChange = (event?: any, type?: string) => {
    const ownerVal = form.getValues("owner"),
      repoVal = form.getValues("repo");
    if (ownerVal.length > 3 && repoVal.length > 3) {
      // let valueVar = type === "owner" ? "repo" : "owner";
      // const formValue = form.getValues(valueVar);
      // if (formValue !== "" || formValue !== null) {
        dispatch(verifyRepoAccess({owner: ownerVal, repo: repoVal}));
      // }
    }
  };

  const confirmConnectModal = () => {
    confirm({ 
      title: "Verify the Repository details", 
      description: "Either the github owner or repository doesn't exist, or this is a private repository. Please proceed to connect the repository if it is a private one. .",
      confirmationText: "Connect"
    }).then(async () => {
        connectToGithub()
      })
      .catch(() => { });
  }

  return (
    <>
      <div id="profileContainer">
        <div>
          <Form form={form} onSubmit={formHandler}>
            {/* <Button buttonLabel={"Connect Github"} displayStyle="primary-outline" onClick={verifyRepo}/> */}
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
              // onChange={onOwnerChange}
              // value={ownerVal}
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
                // onChange={onRepoChange}
                // value={repoVal}
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
      {showConfirmConnection ? confirmConnectModal() : null}
    </>
  );
};

export default UserProfile;
