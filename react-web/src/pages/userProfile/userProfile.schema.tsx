import * as yup from "yup";

export const userProfileSchema = yup.object().shape({
    dappOwner: yup.string().required("This field is required."),
    dappRepository: yup.string().required("This field is required.")
});
