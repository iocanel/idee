FROM scratch

WORKDIR /work/
COPY runner /work/application
RUN chmod 775 /work
EXPOSE 8080 9090
CMD ["./yapp"]
