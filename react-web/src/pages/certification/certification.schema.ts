import * as yup from "yup";

export const certificationSchema = yup.object().shape(
  {
    username: yup.string().required("This field is required."),
    repoName: yup.string().required("This field is required."),
    commit: yup.string().when("branch", {
      is: "",
      then: yup
        .string()
        .min(7, "Please enter a commit hash with length atleast 7")
        .max(40, "Please enter a commit hash with length upto 40")
        .required("This field is required.")
        .matches(
          /[0-9a-f]{7,8}/,
          "Invalid entry. Commit hash must be combination of only lowercase letters and numbers"
        ),
    }),
    branch: yup.string().when("commit", {
      is: (commit: string | any[]) => commit?.length === 0,
      then: yup.string().required("This field is required."),
    }),
  },
  [["commit", "branch"]]
);