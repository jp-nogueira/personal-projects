# -*- coding: utf-8 -*-
"""
Created on Tue Oct 14 13:37:29 2025

@author: JP Nogueira
"""

import pandas as pd
import numpy as np
import nltk
from sentence_transformers import SentenceTransformer, util

# Download sentence tokenizer (only needed once)
nltk.download('punkt')

def bert_extractive_summary_df(df, text_column='text', n_sentences=5):
    """
    Generate an extractive summary from a DataFrame, where each row is a separate text.
    Uses 'all-MiniLM-L6-v2', an English-optimized BERT model for sentence similarity.

    Args:
        df (pd.DataFrame): DataFrame containing the texts.
        text_column (str): Name of the column containing text strings.
        n_sentences (int): Number of sentences to include in the final summary.

    Returns:
        str: Extractive summary generated from the combined texts.
    """

    # 1. Combine all text rows into one corpus
    if text_column not in df.columns:
        raise ValueError(f"Column '{text_column}' not found in DataFrame.")

    texts = df[text_column].dropna().astype(str).tolist()
    combined_text = " ".join(texts)

    # 2. Split combined text into individual sentences
    sentences = nltk.sent_tokenize(combined_text)
    if len(sentences) == 0:
        return ""

    # 3. Load the original BERT model for sentence embeddings
    model = SentenceTransformer('all-MiniLM-L6-v2')

    # 4. Generate embeddings for each sentence
    embeddings = model.encode(sentences, convert_to_tensor=True)

    # 5. Compute the mean embedding (represents central theme)
    mean_embedding = embeddings.mean(dim=0)

    # 6. Compute cosine similarity between each sentence and the mean embedding
    cosine_scores = util.cos_sim(embeddings, mean_embedding)

    # 7. Rank sentences by similarity score (descending)
    ranked_indices = np.argsort(-cosine_scores.cpu().numpy().flatten())

    # 8. Select top N sentences and restore original order
    selected_indices = sorted(ranked_indices[:min(n_sentences, len(sentences))])
    selected_sentences = [sentences[i] for i in selected_indices]

    # 9. Combine selected sentences into a final summary
    summary = " ".join(selected_sentences)
    return summary


# Example usage
if __name__ == "__main__":
    df = pd.read_excel("C:\\Users\\JP Nogueira\\OneDrive - unb.br\\Mestrado\\Dissertação\\ATS\\test.xlsx")
    df = df.iloc[:2]
    

    summary = bert_extractive_summary_df(df, text_column='Text', n_sentences=10)
    print("Extractive Summary:\n", summary)
