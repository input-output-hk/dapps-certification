import React, { useState, useEffect } from "react";
import Button from "components/Button/Button";
import { Input } from "compositions/Form/components/Input";
import { Form } from "compositions/Form/Form";
import { fetchData } from "api/api";
import { getProfileDetails } from "store/slices/auth.slice";
import { useAppDispatch, useAppSelector } from "store/store";
import { useForm } from "hooks/useForm";
import "./UserProfile.scss";
import { userProfileSchema } from "./userProfile.schema";
import { useNavigate } from "react-router-dom";
import { IUserProfile } from "./userProfile.interface";

const UserProfile = () => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const { userDetails, address, wallet, walletName } = useAppSelector((state: any) => state.auth);
  const [isEdit, setIsEdit] = useState(false);
  
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
          "version": formData.version
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

  return (
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
            {...form.register("owner")}
          />
          <Input
            label="dApp Repository"
            disabled={!isEdit}
            type="text"
            required={true}
            disablefocus="true"
            {...form.register("repo")}
          />
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
                  disabled={!form.formState.isValid}
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
  );
};

export default UserProfile;
