import { useEffect, useState } from "react";
import { useForm } from "hooks/useForm";
import { useNavigate } from "react-router-dom";

import Toast from "components/Toast/Toast";
import Button from "components/Button/Button";

import { Form } from "compositions/Form/Form";
import { Input } from "compositions/Form/components/Input";

import { reportUploadSchema } from "./reportUpload.schema";
import DAPPScript from "components/DAPPScript/DAPPScript";
import "./ReportUpload.scss";
import Dropdown from "components/Dropdown/Dropdown";
import TextArea from "components/TextArea/TextArea";
import { useFieldArray } from "react-hook-form";
import { useAppSelector } from "store/store";

export const fieldArrayName: string = "dAppScripts";

const ReportUpload = () => {
  const navigate = useNavigate();
  const { userDetails }  = useAppSelector((state: any) => state.auth);

  useEffect(() => {
    // to be called only once initially
    addNewDappScript()
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  // eslint-disable-next-line
  const [showError, setShowError] = useState("");

  const form = useForm({
    schema: reportUploadSchema,
    mode: "onChange",
  });

  const { fields, append, remove } = useFieldArray({
    control: form.control,
    name: fieldArrayName,
  });

  const addNewDappScript = () => {
    append(
      {
        scriptHash: "",
        contactAddress: "",
      },
      { shouldFocus: true }
    );
  }

  const formHandler = (formData: any) => {
    console.log(formData);
  };

  const initializeFormState = () => {
    const { twitter, website } = userDetails // TBD - subject, name, contact

    // map data into formData structure and form.reset(data)
  }

  // const [files, setFiles] = useState<any>();
  // const uploadReport = () => {};

  const certificationLevelOptions = [
    {
      label: "L0 Audit",
      value: 0,
    },
    {
      label: "L2 Certification",
      value: 2,
    },
  ];


  return (
    <>
      <h2>Upload an Audit Report</h2>
      <div id="auditReportUploadContainer">
        <Form form={form} onSubmit={formHandler}>
          <Dropdown
            options={certificationLevelOptions}
            placeholder="Certification Level"
            required={true}
            onOptionSelect={(option) =>
              form.setValue("certificationLevel", option.label, {
                shouldValidate: true, // trigger on change validation manually
              })
            }
            {...form.register("certificationLevel")}
          />

          <TextArea
            placeholder="Summary"
            required={true}
            minRows={4}
            maxRows={4}
            {...form.register("summary")}
          />

          <TextArea
            placeholder="Disclaimer"
            required={true}
            minRows={4}
            maxRows={4}
            {...form.register("disclaimer")}
          />

          <TextArea
            placeholder="Subject"
            required={true}
            maxRows={2}
            {...form.register("subject")}
          />

          <div className="separator-label">Auditor Information</div>
          <Input
            label="Name"
            type="text"
            id="name"
            required={true}
            {...form.register("name")}
          />

          <Input
            label="Website"
            type="text"
            id="website"
            required={true}
            {...form.register("website")}
          />

          <Input
            label="Email"
            type="text"
            id="email"
            required={true}
            {...form.register("email")}
          />

          <Input
            label="Logo"
            type="text"
            id="logo"
            {...form.register("logo")}
          />

          <Input
            label="Discord"
            type="text"
            id="discord"
            {...form.register("discord")}
          />

          <Input
            label="Twitter"
            type="text"
            id="twitter"
            {...form.register("twitter")}
          />

          <div className="separator-label">Audit Report</div>
          {/* <Upload
            isMultiple={false}
            highlightText="Upload a PDF"
            uploadFiles={(file) => {
              console.log("file", file);
              setFiles(file);
            }}
            showPreview
            tooltipText={`Please upload a ${SUPPORTED_FORMATS.join(",")} file within 5MB size`}
            maxFileSize={FILE_SIZE}
            acceptedTypes={SUPPORTED_FORMATS.join(",")}
            uploadedFiles={files}
            required={true}
            className="bordered"
            onClick={(files) => console.log(files)}
            name="auditReport"
            showDefaultError
          /> */}
          <TextArea
            placeholder="Report URLs"
            required={true}
            maxRows={2}
            tooltipText="Enter comma(,) separatated JSON/PDF URLs or IPFS link corresponding to the report"
            {...form.register("reportURL")}
          />

          <div className="separator-label">DAPP Script</div>
          <div className="relative">
            <div className="absolute action-button addScript-btn">
              <Button
                displayStyle="primary-outline"
                size="small"
                buttonLabel="+ Add Script"
                type="button"
                disabled={
                  !!form?.formState.errors?.[fieldArrayName] || // disable button if errors associated with dynamic form exists
                  form
                    .getValues(fieldArrayName)
                    ?.some(
                      (field: { scriptHash: any; contactAddress: any }) =>
                        !field?.scriptHash || !field?.contactAddress
                    ) // prevent addition of new script boxes if the required field is empty
                }
                onClick={() => { addNewDappScript() }}
              />
            </div>

            {fields.map((field, index) => (
              <DAPPScript
                key={field.id}
                remove={remove}
                value={field}
                index={index}
              />
            ))}
          </div>

          <div className="button-wrapper">
            <Button
              type="button"
              displayStyle="secondary"
              buttonLabel={"Cancel"}
              onClick={() => {
                form.reset();
                navigate(-1);
              }}
            />

            <Button
              // disabled={!form.formState.isValid}
              type="submit"
              buttonLabel={"Submit"}
            />
          </div>
        </Form>
      </div>
      {showError ? <Toast message={showError} /> : null}
    </>
  );
};

export default ReportUpload;
