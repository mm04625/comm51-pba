import streamlit as st
import pandas as pd
import joblib
import os
from PIL import Image

# =====================================================
# Page Configuration
# =====================================================
st.set_page_config(
    page_title="Delivery Risk Prediction Dashboard for DataCo",
    layout="wide"
)

st.title("🚚 Delivery Risk Prediction Dashboard for DataCo")
st.markdown(
    "Predicting late delivery risk to support proactive logistics and operational decision-making."
)

# =====================================================
# Load Model
# =====================================================
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
MODEL_PATH = os.path.join(SCRIPT_DIR, "models", "xgboost_pipeline.pkl")
model = joblib.load(MODEL_PATH)

# =====================================================
# Helper Function — FULL FEATURE VECTOR
# =====================================================
def create_full_input(user_inputs, model):
    """
    Create a full feature vector matching the trained model.
    Missing columns are filled with zeros.
    """
    model_features = model.feature_names_in_
    full_input = pd.DataFrame(0, index=[0], columns=model_features)

    for col, val in user_inputs.items():
        if col in full_input.columns:
            full_input[col] = val

    return full_input

# =====================================================
# Tabs
# =====================================================
tab1, tab2, tab3 = st.tabs([
    "📊 Delivery Performance Overview",
    "🔮 Live Delay Risk Prediction (XGBoost)",
    "🧠 Key Drivers & Recommendations"
])

# =====================================================
# TAB 1 — BUSINESS OVERVIEW
# =====================================================
with tab1:
    st.header("📊 Delivery Performance Overview")

    st.markdown(
        """
        This section summarises historical delivery performance across regions,
        markets, shipping modes, and product categories to highlight where
        delays most frequently occur.
        """
    )

    charts_path = os.path.join(SCRIPT_DIR, "charts")

    def show_image(title, filename):
        st.subheader(title)
        st.image(
            Image.open(os.path.join(charts_path, filename)),
            use_container_width=True
        )

    col1, col2 = st.columns(2)
    with col1:
        show_image(
            "Distribution of Delivery Outcomes",
            "Distribution_of_delivery_outcome.png"
        )
    with col2:
        show_image(
            "Late Delivery Rate by Shipping Mode",
            "Late_delivery_rate_shipping_mode.png"
        )

    col3, col4 = st.columns(2)
    with col3:
        show_image(
            "Region-wise Sales by Delivery Status",
            "Region_wise.png"
        )
    with col4:
        show_image(
            "Market-wise Sales by Delivery Status",
            "Market_wise.png"
        )

    show_image(
        "Product Categories with Highest Share of Late Orders",
        "Late_products.png"
    )

    st.markdown(
        """
        **Key Business Insights**
        - Delivery delays vary significantly by region and shipping mode.
        - Same-day shipping exhibits the highest late delivery risk.
        - Certain product categories account for a disproportionate share of late orders.
        - These patterns support targeted, data-driven logistics interventions.
        """
    )

# =====================================================
# TAB 2 — LIVE PREDICTION (MLP)
# =====================================================
with tab2:
    st.header("🔮 Live Delivery Delay Risk Prediction")

    st.markdown(
        """
        This simulator uses the **XGBoost** model — the
        **best-performing model** in this study — to estimate whether an order
        is likely to be delivered late *before dispatch*.

        Adjust the key operational factors below to explore different delivery scenarios.
        """
    )

    st.subheader("Select Order Scenario")

    col1, col2 = st.columns(2)

    with col1:
        processing_days = st.slider(
            "Warehouse Processing Time (Days)",
            min_value=0.0,
            max_value=7.0,
            value=2.0,
            step=0.5
        )

        distance_km = st.slider(
            "Delivery Distance (km)",
            min_value=0.0,
            max_value=10000.0,
            value=500.0,
            step=100.0
        )

    with col2:
        shipping_mode = st.selectbox(
            "Shipping Mode",
            ["Standard Class", "Second Class", "Same Day"]
        )

        region = st.selectbox(
            "Order Region",
            [
                "US Center", "East of USA", "West of USA",
                "Western Europe", "Eastern Europe",
                "South Asia", "Southeast Asia",
                "South America", "Central America",
                "Africa", "North Africa", "East Africa",
                "West Africa", "Central Africa",
                "Northern Europe", "Southern Europe",
                "Southern Africa", "Caribbean",
                "Oceania", "West Asia", "Eastern Asia",
                "South of USA"
            ]
        )

    # -------------------------------
    # Build User Input Dictionary
    # -------------------------------
    user_inputs = {
        "Processing.Days": processing_days,
        "Distance_km": distance_km,
        "WeekendOrder": 0,
        "`Order Item Quantity`": 1,
        "`Order Item Discount Rate`": 0.0,
        "`Order Item Product Price`": 100.0,
    }

    # Shipping mode encoding
    user_inputs["`Shipping Mode`Same Day"] = 1 if shipping_mode == "Same Day" else 0
    user_inputs["`Shipping Mode`Second Class"] = 1 if shipping_mode == "Second Class" else 0
    user_inputs["`Shipping Mode`Standard Class"] = 1 if shipping_mode == "Standard Class" else 0

    # Region encoding
    region_columns = [
        "`Order Region`US Center",
        "`Order Region`East of USA",
        "`Order Region`West of USA",
        "`Order Region`Western Europe",
        "`Order Region`Eastern Europe",
        "`Order Region`South Asia",
        "`Order Region`Southeast Asia",
        "`Order Region`South America",
        "`Order Region`Central America",
        "`Order Region`Africa",
        "`Order Region`North Africa",
        "`Order Region`East Africa",
        "`Order Region`West Africa",
        "`Order Region`Central Africa",
        "`Order Region`Northern Europe",
        "`Order Region`Southern Europe",
        "`Order Region`Southern Africa",
        "`Order Region`Caribbean",
        "`Order Region`Oceania",
        "`Order Region`West Asia",
        "`Order Region`Eastern Asia",
        "`Order Region`South of USA",
    ]

    for col in region_columns:
        user_inputs[col] = 1 if col.endswith(region) else 0

    # Create full model input
    input_data = create_full_input(user_inputs, model)

    if st.button("Predict Delivery Risk"):
        probability = model.predict_proba(input_data)[0][1]

        if probability < 0.30:
            risk_label = "🟢 Low Risk"
        elif probability < 0.60:
            risk_label = "🟠 Medium Risk"
        else:
            risk_label = "🔴 High Risk"

        st.subheader("Prediction Result")
        st.metric(
            label="Probability of Late Delivery",
            value=f"{probability:.1%}"
        )
        st.markdown(f"### Risk Category: {risk_label}")

# =====================================================
# TAB 3 — DRIVERS & RECOMMENDATIONS
# =====================================================
with tab3:
    st.header("🧠 Key Drivers of Delay & Strategic Recommendations")

    st.markdown(
        """
        Analysis of the XGBoost model highlights several operational factors
        that consistently increase the likelihood of delivery delays.
        """
    )

    st.subheader("Primary Drivers of Delivery Delays")
    st.markdown(
        """
        - **Warehouse Processing Time**: Longer internal handling times sharply increase delay risk.
        - **Shipping Mode Selection**: Same-day and expedited shipping modes show higher failure rates.
        - **Delivery Distance**: Longer transport distances increase exposure to disruptions.
        - **Geographic Region**: Certain regions face structural logistics challenges.
        """
    )

    st.subheader("Recommendations for Senior Management")
    st.markdown(
        """
        **Operational Actions**
        - Prioritise high-risk orders for accelerated processing.
        - Restrict same-day shipping to low-risk scenarios.
        - Introduce region-specific logistics contingency planning.

        **Strategic Actions**
        - Integrate delivery risk scoring into order routing systems.
        - Proactively manage customer expectations for high-risk orders.
        - Continuously retrain the model as new data becomes available.
        """
    )

    st.success(
        "Embedding this predictive system into logistics workflows enables "
        "DataCo to reduce late deliveries, improve customer satisfaction, "
        "and optimise fulfilment efficiency."
    )
