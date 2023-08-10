import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import { fetchData } from "api/api";
import { LocalStorageKeys } from "constants/constants";
import { IUserProfile } from "pages/userProfile/userProfile.interface";

// Define a type for the slice state
interface AuthState {
  isLoggedIn: boolean;
  address: string;
  wallet: any;
  userDetails: IUserProfile;
  loading: boolean;
  network: number | null;
  subscribedFeatures: Array<"l1-run" | "l2-upload-report"> | null;
}

// Define the initial state using that type
const initialState: AuthState = {
  isLoggedIn: false,
  address: '',
  wallet: null,
  userDetails: {dapp: null},
  loading: false,
  network: null,
  subscribedFeatures: null,
};

const clearLSCache = () => {
  localStorage.clear();
}

export const getProfileDetails: any = createAsyncThunk("getProfileDetails", async (data: any, { rejectWithValue }) => {
  localStorage.setItem(LocalStorageKeys.address, data.address) 
  const response = await fetchData.get("/profile/current", data)
  // FOR MOCK - const response = await fetchData.get(data.url || 'static/data/current-profile.json', data)
  return response.data
})

export const authSlice = createSlice({
  name: "auth",
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    clearCache: (state) => {
      clearLSCache();
    },
    logout: (state) => {
      clearLSCache();
      state.loading = false;
      return initialState
    },
    setNetwork: (state, actions) => {
      state.network = actions.payload
    },
    setSubscribedFeatures: (state, actions) => {
      state.subscribedFeatures = actions.payload
    }
  },
  extraReducers: (builder) => {
    builder.addCase(getProfileDetails.pending, (state) => {state.loading = true;})
      .addCase(getProfileDetails.fulfilled, (state, actions) => {
        state.loading = false;
        state.isLoggedIn = true;
        state.userDetails = actions.payload;
        localStorage.setItem(LocalStorageKeys.isLoggedIn, "true");
        if (actions?.meta?.arg?.address) {
          state.address = actions.meta.arg.address;
          localStorage.setItem(LocalStorageKeys.address, state.address)
          if (actions?.meta?.arg?.wallet) {
            state.wallet = actions.meta.arg.wallet;
          }
          if (actions?.meta?.arg?.walletName) {
            localStorage.setItem(LocalStorageKeys.walletName, actions?.meta?.arg?.walletName)
          }
        }
      })
      .addCase(getProfileDetails.rejected, (state) => {
        clearLSCache()
        return initialState
      })
  }
});


export const { logout, clearCache, setNetwork, setSubscribedFeatures} = authSlice.actions;

export default authSlice.reducer;