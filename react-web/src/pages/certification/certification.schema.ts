import * as yup from "yup";

export const certificationSchema = yup.object().shape(
  {
    username: yup.string().required("This field is required."),
    repoName: yup.string().required("This field is required."),
    commit: yup.string().when("branch", {
      is: "",
      then: yup
        .string()
        .min(6)
        .max(40)
        .required("This field is required.")
        .matches(/[a-z0-9]{6}/),
    }),
    branch: yup.string().when("commit", {
      is: (commit: string | any[]) => commit?.length === 0,
      then: yup.string().required("This field is required."),
    }),
  },
  [["commit", "branch"]]
);