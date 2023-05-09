import { useState } from "react"
import { useForm } from "hooks/useForm"

import Toast from "components/Toast/Toast"
import Button from "components/Button/Button"

import { Form } from "compositions/Form/Form"
import { Input } from "compositions/Form/components/Input"

import { reportUploadSchema } from "./reportUpload.schema"
import { IAuditReport } from "./reportUpload.interface"
import DAPPScript from "components/DAPPScript/DAPPScript"
import './ReportUpload.scss'

const ReportUpload = () => {


    const [showError, setShowError] = useState("")

    const form: any = useForm({
        schema: reportUploadSchema,
        mode: "onChange",
    });

    const formHandler = (formData: any) => {
        
    }

    const uploadReport = () => {

    }

    return (<>
        <h2>Upload an Audit Report</h2>
        <div id="auditReportUploadContainer">
            <Form form={form} onSubmit={formHandler}>
                <Input
                    label="Certification Level"
                    type="text"
                    id="certificationLevel"
                    required={true}
                    {...form.register("certificationLevel")} />
                <Input
                    label="Summary"
                    type="text"
                    id="summary"
                    required={true}
                    {...form.register("summary")} />
                <Input
                    label="Disclaimer"
                    type="text"
                    id="disclaimer"
                    required={true}
                    {...form.register("disclaimer")} />
                <Input
                    label="Subject"
                    type="text"
                    id="subject"
                    required={true}
                    {...form.register("subject")} />
                <div className="separator-label">Auditor Information</div>
                <Input
                    label="Name"
                    type="text"
                    id="name"
                    required={true}
                    {...form.register("name")} />
                <Input
                    label="Website"
                    type="text"
                    id="website"
                    required={true}
                    {...form.register("website")} /> 
                <Input
                    label="Email"
                    type="text"
                    id="email"
                    required={true}
                    {...form.register("email")} />                    
                <Input
                    label="Logo"
                    type="text"
                    id="logo"
                    {...form.register("logo")} />
                <Input
                    label="Discord"
                    type="text"
                    id="discord"
                    {...form.register("discord")} />
                <Input
                    label="Twitter"
                    type="text"
                    id="twitter"
                    {...form.register("twitter")} />
                <div className="separator-label">Audit Report</div>
                <div className="bordered">
                    <label>Upload a PDF (max size 5MB)</label>
                    <Button 
                        displayStyle="gradient"
                        size="thin"
                        buttonLabel="Upload"
                        type="button"
                        onClick={()=> { uploadReport() }}/>
                </div>
                <div className="separator-label">DAPP Script</div>
                <DAPPScript />

                <div className="button-wrapper">
                    <Button
                        type="button"
                        displayStyle="secondary"
                        buttonLabel={"Cancel"}
                        onClick={() => {
                        //   localStorage.removeItem('profile')
                        //   initializeFormState()
                        //   setIsEdit(false);
                        }}
                    />
                    <Button
                        disabled={!form.formState.isValid}
                        type="submit"
                        buttonLabel={"Submit"}
                    />
                </div>
            </Form>
        </div>
        {showError ? <Toast message={showError} /> : null}
    </>)
}

export default ReportUpload