autoupdate:
	@echo "Syncing changes to Git remote..."
	git add .
	@if [ -n "$$(git status --porcelain)" ]; then \
		git commit -m "auto: update pipeline and results [$$(date +'%Y-%m-%d %H:%M:%S')]"; \
		git push origin main; \
		echo "Successfully updated remote repository."; \
	else \
		echo "No changes detected to commit."; \
	fi
