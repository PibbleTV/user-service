package com.pibbletv.user_service;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;

@Configuration
public class PublicKeyConfig {

    @Value("${KEYCLOAK_PUBLIC_KEY}")
    private String publicKeyPEM;

    @Bean
    public PublicKey publicKey() {
        try {

            byte[] decoded = Base64.getDecoder().decode(publicKeyPEM);
            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(decoded);
            return KeyFactory.getInstance("RSA").generatePublic(keySpec);
        } catch (Exception e) {
            throw new RuntimeException("Failed to decode public key", e);
        }
    }
}
