# ====================================================
# Fine-Tuned DistilBERT for Per-Admission Clinical Prediction
# ====================================================

# --------------------------
# 📚 Package Imports
# --------------------------
import pandas as pd
import numpy as np
from datasets import Dataset
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification, Trainer, TrainingArguments
from sklearn.metrics import accuracy_score, f1_score, roc_auc_score

# --------------------------
# 📂 Load Data
# --------------------------
train_df = pd.read_csv("../data/train.csv")
test_df = pd.read_csv("../data/test.csv")
print("Train sample:")
print(train_df.head())

# --------------------------
# Data Preparation for 'hospital_expire_flag'
# --------------------------
# Drop label-leaking features
train_df.drop(columns="days_until_death", inplace=True, errors='ignore')
test_df.drop(columns="days_until_death", inplace=True, errors='ignore')

label_col = "hospital_expire_flag"
feature_cols = [c for c in train_df.columns if c != label_col]

def row_to_text(row):
    """Convert a row into a single string with feature=value pairs."""
    return " | ".join([f"{col}={row[col]}" for col in feature_cols])

train_df["text"] = train_df.apply(row_to_text, axis=1)
test_df["text"] = test_df.apply(row_to_text, axis=1)

print(train_df[["text", label_col]].head())

# --------------------------
# Conversion to HuggingFace Dataset
# --------------------------
train_ds = Dataset.from_pandas(train_df[["text", label_col]])
test_ds = Dataset.from_pandas(test_df[["text", label_col]])

train_ds = train_ds.rename_column(label_col, "label")
test_ds = test_ds.rename_column(label_col, "label")

# --------------------------
# 🔤 Tokenisation
# --------------------------
model_name = "distilbert-base-uncased"
tokenizer = AutoTokenizer.from_pretrained(model_name)

def tokenize(batch):
    return tokenizer(batch["text"], truncation=True, padding=True, max_length=256)

train_ds = train_ds.map(tokenize, batched=True)
test_ds = test_ds.map(tokenize, batched=True)

train_ds.set_format("torch", columns=["input_ids", "attention_mask", "label"])
test_ds.set_format("torch", columns=["input_ids", "attention_mask", "label"])

# --------------------------
# 🧠 Load Model
# --------------------------
model = AutoModelForSequenceClassification.from_pretrained(model_name, num_labels=2)

# --------------------------
# 📏 Metrics
# --------------------------
def compute_metrics(eval_pred):
    logits, labels = eval_pred
    probs = torch.softmax(torch.tensor(logits), dim=1)[:, 1].numpy()
    preds = np.argmax(logits, axis=1)
    return {
        "accuracy": accuracy_score(labels, preds),
        "f1": f1_score(labels, preds),
        "roc_auc": roc_auc_score(labels, probs),
    }

# --------------------------
# ⚙️ Training Arguments
# --------------------------
training_args = TrainingArguments(
    output_dir="./llm_output",
    logging_strategy="epoch",
    eval_strategy="epoch",
    save_strategy="epoch",
    learning_rate=2e-3,
    per_device_train_batch_size=16,
    per_device_eval_batch_size=16,
    num_train_epochs=3,
    weight_decay=0.01,
    logging_dir="./llm_logs",
    load_best_model_at_end=True,
    metric_for_best_model="roc_auc"
)


# --------------------------
# Save Model
# --------------------------
model.save_pretrained("./llm_model")
tokenizer.save_pretrained("./llm_model")
# --------------------------