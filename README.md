# COMM053-PBA COURSEWORK



## Getting started

To make it easy for you to get started with GitLab, here's a list of recommended next steps.

Already a pro? Just edit this README.md and make it your own. Want to make it easy? [Use the template at the bottom](#editing-this-readme)!

## Add your files

- [ ] [Create](https://docs.gitlab.com/ee/user/project/repository/web_editor.html#create-a-file) or [upload](https://docs.gitlab.com/ee/user/project/repository/web_editor.html#upload-a-file) files
- [ ] [Add files using the command line](https://docs.gitlab.com/topics/git/add_files/#add-files-to-a-git-repository) or push an existing Git repository with the following command:

```
cd existing_repo
git remote add origin https://gitlab.surrey.ac.uk/mm04625/comm053-pba.git
git branch -M main
git push -uf origin main
```

## Integrate with your tools

- [ ] [Set up project integrations](https://gitlab.surrey.ac.uk/mm04625/comm053-pba/-/settings/integrations)

## Collaborate with your team

- [ ] [Invite team members and collaborators](https://docs.gitlab.com/ee/user/project/members/)
- [ ] [Create a new merge request](https://docs.gitlab.com/ee/user/project/merge_requests/creating_merge_requests.html)
- [ ] [Automatically close issues from merge requests](https://docs.gitlab.com/ee/user/project/issues/managing_issues.html#closing-issues-automatically)
- [ ] [Enable merge request approvals](https://docs.gitlab.com/ee/user/project/merge_requests/approvals/)
- [ ] [Set auto-merge](https://docs.gitlab.com/user/project/merge_requests/auto_merge/)

## Test and Deploy

Use the built-in continuous integration in GitLab.

- [ ] [Get started with GitLab CI/CD](https://docs.gitlab.com/ee/ci/quick_start/)
- [ ] [Analyze your code for known vulnerabilities with Static Application Security Testing (SAST)](https://docs.gitlab.com/ee/user/application_security/sast/)
- [ ] [Deploy to Kubernetes, Amazon EC2, or Amazon ECS using Auto Deploy](https://docs.gitlab.com/ee/topics/autodevops/requirements.html)
- [ ] [Use pull-based deployments for improved Kubernetes management](https://docs.gitlab.com/ee/user/clusters/agent/)
- [ ] [Set up protected environments](https://docs.gitlab.com/ee/ci/environments/protected_environments.html)

***

## Overview

This project focuses on solving a binary classification problem using supervised machine learning techniques. A pre-processed business dataset is analysed to predict the target variable **Late_delivery_risk**. The objective is to build, evaluate, and compare different machine learning models in order to identify the most suitable approach based on performance, robustness, and interpretability.

The project follows a structured machine learning workflow including data loading, model training, hyperparameter tuning, evaluation, and comparison. All modelling and evaluation steps are implemented in Python using the `scikit-learn` library.

---

## Dataset

* **File**: `data_model_preprocessing-PBA.csv`
* **Target Variable**: `Late_delivery_risk` (binary classification)
* **Features**: All remaining columns after removing the target

The dataset has already undergone cleaning, encoding, and transformation prior to modelling. No additional preprocessing steps (such as handling missing values or encoding categorical variables) are performed in Python.

---

## Models Implemented

### Logistic Regression

Logistic Regression is used as a baseline linear classification model. It estimates the probability of a binary outcome using a logistic (sigmoid) function and is well-suited for problems where the relationship between features and the target is approximately linear.

**Key characteristics:**

* Produces probabilistic outputs
* Less prone to overfitting due to regularisation
* Highly interpretable

Hyperparameter tuning is not required because the default parameters provided a good result.

---

### Decision Tree Classifier

The Decision Tree Classifier is a non-linear model that splits the data into regions based on feature thresholds. It can capture complex patterns and interactions between variables but is more susceptible to overfitting if not properly constrained.

**Key characteristics:**

* Captures non-linear relationships
* Easy to interpret visually
* High variance without regularisation

Hyperparameters controlling tree depth and node splitting are tuned to reduce overfitting.

---

## Logistic Regression vs Decision Tree

Both models are trained and evaluated using the same training and test datasets to ensure a fair comparison. Hyperparameter tuning is performed using cross-validation, and identical evaluation metrics are used for decision tree model.

### Comparison Criteria

* **Accuracy**: Overall correctness of predictions
* **Precision**: Proportion of correctly predicted positive cases
* **Recall**: Ability to identify actual positive cases
* **F1-Score**: Balance between precision and recall
* **ROC-AUC**: Ability to distinguish between classes across thresholds

### Key Observations

* Logistic Regression demonstrates more stable and consistent performance across most metrics, particularly ROC-AUC.
* Decision Tree achieves competitive recall but shows slightly lower overall generalisation performance.
* Logistic Regression benefits from regularisation, resulting in better control over model complexity.
* Decision Tree performance is more sensitive to hyperparameter choices due to its flexible structure.

Visual comparisons include:

* Confusion matrices for each model
* ROC curves plotted on the same figure
* Bar charts comparing evaluation metrics
