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
  network: number | null;
}

// Define the initial state using that type
const initialState: AuthState = {
  isLoggedIn: false,
  address: '',
  wallet: null,
  userDetails: {dapp: null},
  loading: false,
  network: null
};

const clearLSCache = () => {
  localStorage.removeItem('address')
  localStorage.removeItem('walletName')
  localStorage.removeItem('authToken')
}

export const getProfileDetails: any = createAsyncThunk("getProfileDetails", async (data: any, { rejectWithValue }) => {
  localStorage.setItem('address', data.address) 
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
    }
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
          if (actions?.meta?.arg?.walletName) {
            localStorage.setItem('walletName', actions?.meta?.arg?.walletName)
          }
        }
      })
      .addCase(getProfileDetails.rejected, (state) => {
        clearLSCache()
        return initialState
      })
  }
});


export const { logout, clearCache, setNetwork } = authSlice.actions;

export default authSlice.reducer;
