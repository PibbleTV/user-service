package com.pibbletv.user_service.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class User {
    private Long id;

    private UUID userId;

    private String username;

    private String bgImage;

    private String profileImage;

    private Boolean isBanned;
}
