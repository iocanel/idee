FROM scratch

WORKDIR /work/
COPY runner /work/application
RUN chmod 775 /work
EXPOSE 8080
CMD ["./yapp"]
