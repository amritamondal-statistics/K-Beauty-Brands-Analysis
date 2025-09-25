import pandas as pd
import streamlit as st
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import urllib.parse

# ------------------ Load Dataset ------------------
url="https://raw.githubusercontent.com/amritamondal-statistics/K-Beauty-Brands-Analysis/main/data/cosmetics.csv"
cosmetics = pd.read_csv(url)
cosmetics["combined_features"] = cosmetics["Product Name"] + " " + cosmetics["Main Ingredients"]

# ------------------ TF-IDF & Cosine Similarity ------------------
vectorizer = TfidfVectorizer(stop_words="english")
tfidf_matrix = vectorizer.fit_transform(cosmetics["combined_features"])
cosine_sim = cosine_similarity(tfidf_matrix, tfidf_matrix)


# ------------------ Recommendation Function ------------------
def recommend_products(product_name, product_type=None, top_n=5):
    matches = cosmetics[cosmetics["Product Name"] == product_name]
    if matches.empty:
        return pd.DataFrame({"Message": ["Product not found!"]})

    idx = matches.index[0]
    sim_scores = list(enumerate(cosine_sim[idx]))
    sim_scores = sorted(sim_scores, key=lambda x: x[1], reverse=True)

    recommendations = []
    for i, score in sim_scores:
        candidate = cosmetics.iloc[i]
        if candidate["Product Name"] != product_name:
            if (product_type is None) or (candidate["Product Type"] == product_type):
                recommendations.append({
                    "Product Name": candidate["Product Name"],
                    "Company Name": candidate.get("Company Name", "N/A"),
                    "Rating": candidate.get("Rating", "N/A"),
                    "Main Ingredients": candidate.get("Main Ingredients", "N/A")
                })
        if len(recommendations) >= top_n:
            break
    return pd.DataFrame(recommendations)


# ------------------ Streamlit UI ------------------
st.set_page_config(page_title="Cosmetic Recommendation", layout="wide")
st.title("💄 Cosmetic Product Recommendation System")

# Product Type selection
product_types = cosmetics["Product Type"].unique().tolist()
selected_type = st.selectbox("Select Product Type:", ["All"] + product_types)

# Product selection
if selected_type != "All":
    product_list = cosmetics[cosmetics["Product Type"] == selected_type]["Product Name"].tolist()
else:
    product_list = cosmetics["Product Name"].tolist()

product_input = st.selectbox("Select a Product:", product_list)

# Top N slider
Top_n = st.slider("How many recommendations?", 1, 10, 5)

# Button to get recommendations
if st.button("Get Recommendations"):
    results_df = recommend_products(product_input, None if selected_type == "All" else selected_type, Top_n)

    st.write("### Recommended Products:")
    for _, row in results_df.iterrows():
        st.markdown(f"**{row['Product Name']}**  ")
        st.markdown(f"Brand: {row['Company Name']} | Rating: {row['Rating']}⭐  ")
        st.markdown(f"Ingredients: {row['Main Ingredients']}  ")

        # Dynamic Google shopping link
        query = urllib.parse.quote(f"{row['Product Name']} {row['Company Name']} buy online")
        google_link = f"https://www.google.com/search?q={query}"
        st.markdown(f"[Shop Now]({google_link})", unsafe_allow_html=True)

        st.markdown("---")
