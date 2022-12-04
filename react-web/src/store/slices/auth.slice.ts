import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import { postData } from "api/api";

// Define a type for the slice state
interface AuthState {
  isLoggedIn: boolean;
  address: string;
  userDetails: {
    company?: string;
    vendor?: string;
    website?: string;
    linkedIn?: string;
    dappOwner?: string; //will be received only if pre-set
    dappRepository?: string; //will be received only if pre-set
  };
  loading: boolean;
}

// Define the initial state using that type
const initialState: AuthState = {
  isLoggedIn: false,
  address: '',
  userDetails: {},
  loading: false
};

export const getProfileDetails: any = createAsyncThunk("getProfileDetails", async (data: any, { rejectWithValue }) => {
  try {  
    // const response = await postData.get("/profile/current")
    const response = await postData.get(data.url || 'static/data/current-profile.json', data)
    return response.data
  } catch(e) {
    return rejectWithValue(e)
  }
})

export const authSlice = createSlice({
  name: "auth",
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    logout: (state) => {
      localStorage.removeItem('address')
      return initialState
    },
  },
  extraReducers: {
    [getProfileDetails.pending]: (state) => {state.loading = true;},
    [getProfileDetails.fulfilled]: (state, actions) => {
      state.loading = false;
      state.isLoggedIn = true;
      state.userDetails = actions.payload;
      state.address = actions.meta.arg.address;
      localStorage.setItem('address', state.address)
    },
    [getProfileDetails.rejected]: (state) => {
      localStorage.removeItem('address')
      return initialState
    }
  }
});


export const { logout } = authSlice.actions;

export default authSlice.reducer;
