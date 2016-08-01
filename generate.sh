auth-token-swagger-gen swagger.json docs.markdown
pandoc docs.markdown -o docs.html
git add swagger.json docs.html
git commit -m "Update documentation"
git push origin gh-pages