import { createSlice } from "@reduxjs/toolkit";

// Define a type for the slice state
interface CertificationState {
  uuid: string;
}

// Define the initial state using that type
const initialState: CertificationState = {
  uuid: "",
};

export const certificationSlice = createSlice({
  name: "certification",
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    setUuid: (state, action) => {
      state.uuid = action.payload;
    },
    clearUuid: () => initialState,
  },
});

export const { setUuid, clearUuid } = certificationSlice.actions;

export default certificationSlice.reducer;