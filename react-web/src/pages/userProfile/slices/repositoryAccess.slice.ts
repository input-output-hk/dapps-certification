import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import { getRepoAccess, postExternal } from "api/api";

interface RepoAccessState {
    verifying: boolean,
    accessible: boolean,
    showConfirmConnection: boolean,
    accessToken: string
}
const initialState: RepoAccessState = {
    verifying: false,
    accessible: false,
    showConfirmConnection: false,
    accessToken: ""
};


export const verifyRepoAccess = createAsyncThunk(
    "verifyRepoAccess",
    async (payload: any, { rejectWithValue }) => {
      try {
        const response = await getRepoAccess.get('/repo/' + payload.owner + '/' + payload.repo);
        return response.data;
      } catch (e: any) {
        return rejectWithValue(e.response.data);
      }
    }
  );


export const getUserAccessToken = createAsyncThunk("getUserAccessToken", async(payload: any, {rejectWithValue}) => {
    const response: any = postExternal.post("https://github.com/login/oauth/access_token" + payload.queryParams)
    return response
})  

export const repoAccessSlice = createSlice({
  name: "repoAccess",
  initialState,
  reducers: {
    clearStates: () => {
        localStorage.removeItem('accessToken')
        localStorage.removeItem('profile')
        return initialState
    },
    clearAccessToken: (state) => {
      state.accessToken = ""
    }
  },
  extraReducers: (builder) => {
    builder
      .addCase(verifyRepoAccess.rejected, (state, actions) => {
        state.verifying = false;
        state.accessible = false;
        state.showConfirmConnection = true;
      })
      .addCase(verifyRepoAccess.pending, (state, actions) => {
        state.verifying = true;
        state.showConfirmConnection = false;
      })
      .addCase(verifyRepoAccess.fulfilled, (state, actions) => {
        state.accessible = true;
        state.verifying = false;
        state.showConfirmConnection = false;
      })
      .addCase(getUserAccessToken.rejected, (state, actions) => {
        state.accessToken = ""
      })
      .addCase(getUserAccessToken.pending, (state, actions) => {
        
      })
      .addCase(getUserAccessToken.fulfilled, (state, actions) => { console.log('payload-', actions.payload)
        const token: string = actions.payload?.data.access_token
        localStorage.setItem('accessToken', token)
        state.accessToken = token;
      });
  },
});
export const { clearStates, clearAccessToken } = repoAccessSlice.actions;

export default repoAccessSlice.reducer;
