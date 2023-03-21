import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import { getRepoAccess, postExternal } from "api/api";

interface RepoAccessState {
    verifying: boolean,
    accessible: boolean,
    showConfirmConnection: boolean
}
const initialState: RepoAccessState = {
    verifying: false,
    accessible: false,
    showConfirmConnection: false
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
    if(response.data.access_token) {
        return response.data.access_token
    } else {
        return rejectWithValue('Something wrong occurred!')
    }
})  

export const repoAccessSlice = createSlice({
  name: "repoAccess",
  initialState,
  reducers: {
    clearStates: () => {
        localStorage.removeItem('accessToken')
        return initialState
    }
  },
  extraReducers: (builder) => {
    builder
      .addCase(verifyRepoAccess.rejected, (state, actions) => {
        state.verifying = false;
        state.accessible = false;
        state.showConfirmConnection = true;
        console.log('failed repo access - ', actions)
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

        })
      .addCase(getUserAccessToken.pending, (state, actions) => {
        
      })
      .addCase(getUserAccessToken.fulfilled, (state, actions) => {
        const token: string = actions.payload
        console.log('fulfilled - ', token)
        localStorage.setItem('accessToken', token)
      });
  },
});
export const { clearStates } = repoAccessSlice.actions;

export default repoAccessSlice.reducer;
