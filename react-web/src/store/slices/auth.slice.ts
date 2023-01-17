import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import { fetchData } from "api/api";
import { IUserProfile } from "pages/userProfile/userProfile.interface";

// Define a type for the slice state
interface AuthState {
  isLoggedIn: boolean;
  address: string;
  wallet: any;
  userDetails: IUserProfile;
  loading: boolean;
}

// Define the initial state using that type
const initialState: AuthState = {
  isLoggedIn: false,
  address: '',
  wallet: null,
  userDetails: {dapp: null},
  loading: false
};

export const getProfileDetails: any = createAsyncThunk("getProfileDetails", async (data: any, { rejectWithValue }) => {
  try { 
    localStorage.setItem('address', data.address) 
    const response = await fetchData.get("/profile/current", data)
    // FOR MOCK - const response = await fetchData.get(data.url || 'static/data/current-profile.json', data)
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
  extraReducers: (builder) => {
    builder.addCase(getProfileDetails.pending, (state) => {state.loading = true;})
      .addCase(getProfileDetails.fulfilled, (state, actions) => {
        state.loading = false;
        state.isLoggedIn = true;
        state.userDetails = actions.payload;
        if (actions?.meta?.arg?.address) {
          state.address = actions.meta.arg.address;
          localStorage.setItem('address', state.address)
          if (actions?.meta?.arg?.wallet) {
            state.wallet = actions.meta.arg.wallet;
          }
        }
      })
      .addCase(getProfileDetails.rejected, (state) => {
        localStorage.removeItem('address')
        return initialState
      })
  }
});


export const { logout } = authSlice.actions;

export default authSlice.reducer;
