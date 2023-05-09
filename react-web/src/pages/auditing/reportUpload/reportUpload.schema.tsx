import * as yup from "yup";

export const reportUploadSchema = yup.object().shape({
    name: yup.string().required("This field is required."),
    version: yup.string().required("This field is required."),
});
