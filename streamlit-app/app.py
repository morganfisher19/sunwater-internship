# Import packages
import pandas as pd
import streamlit as st
import streamlit.components.v1 as components
import pygwalker as pyg
from pygwalker.api.streamlit import StreamlitRenderer

# Page Settings
st.set_page_config(
    page_title="Data Dashboard",
    page_icon=":classical_building:",
    layout="wide",
    initial_sidebar_state="expanded",
)

# Title & subtitle of page
st.title("Sunwater Intern Data Analytics Dashboard")
st.subheader('Upload your dataset here and create a dashboard.')

# Add a paragraph and a link
st.markdown("""
    <p>
        Welcome to the Data Dashboard. Here, you can upload your dataset and use various tools to generate insightful charts and graphs.
        For instructions on how to use this tool, please refer to the following link: 
        <a href="https://help.tableau.com/current/pro/desktop/en-us/default.htm8" target="_blank">TABLEAU DOCUMENTATION</a>
        <br>
        <br>
        For further guidance, please watch the following video: 
        <a href="https://youtu.be/jEgVto5QME8?t=365" target="_blank">DASHBOARD TUTORIAL</a> (you can ignore the first 6 minutes)
    </p>
    """, unsafe_allow_html=True)

# Uploading Data
uploaded_file = st.file_uploader("your csv data (MUST be .csv file)")

if uploaded_file is not None:
    df = pd.read_csv(uploaded_file)
    renderer = StreamlitRenderer(df, spec="./gw_config.json", spec_io_mode="rw")
    renderer.explorer()



