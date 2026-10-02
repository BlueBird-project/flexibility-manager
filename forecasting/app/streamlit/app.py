import streamlit as st
from streamlit_option_menu import option_menu
import os
from pathlib import Path
from pages.pages import PageInit, PageView, PageForecasting


if "streamlit" in os.getcwd():
    parent_dir = Path.cwd().parent.parent
elif "app" in os.getcwd():
    parent_dir = Path.cwd().parent
else:
    parent_dir = Path.cwd()

st.set_page_config(page_title="BlueBird Forecasting", 
                   page_icon=os.path.join(parent_dir,"src", r"bluebird-logo-V8.png"),
                   initial_sidebar_state= "collapsed",
                   layout= "wide",
                   menu_items= {
                       "Get help": "https://github.com/BlueBird-project"
                   })

if "data" not in st.session_state or st.session_state["data"] is None:

    PageInit(parent_dir)

else:

    data = st.session_state["data"]

    selected3 = option_menu("",["View Data", "Inference"], 
                    icons=['file-bar-graph-fill', 'motherboard-fill'], 
                    menu_icon="cast", default_index=0, orientation="horizontal",
                    styles={
                        "container": {"padding": "0!important", "background-color": "#399a55"},
                        "icon": {"color": "#ffffff", "font-size": "25px"}, 
                        "nav-link": {"font-size": "25px", "text-align": "left", "margin":"0px", "--hover-color": "#10a8e3"},
                        "nav-link-selected": {"background-color": "#0e2e45"},
                    }
                )

    if selected3 == "View Data":
        
        PageView(data)

    elif selected3 == "Inference":
        PageForecasting(data)

    if st.button("Close"):
        st.session_state["data"] = None
        st.rerun()  
