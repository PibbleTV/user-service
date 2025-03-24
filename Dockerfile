# Use Oracle Linux 8 as the base image
FROM oraclelinux:8

# Install OpenJDK
RUN dnf install -y java-17-openjdk

# Set the working directory inside the container
WORKDIR /app

# Copy the compiled JAR file into the container
COPY build/libs/user_service-0.0.1-SNAPSHOT.jar user_service.jar

# Expose the application's port
EXPOSE 8081

# Run the application
ENTRYPOINT ["java", "-jar", "user_service.jar"]

#docker build -t user-service -f Dockerfile .
#docker run -d --name user-service -p 8081:8081 service-registry